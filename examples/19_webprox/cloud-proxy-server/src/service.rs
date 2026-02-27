use crate::browser::BrowserPool;
use crate::compressor::PatchCompressor;
use crate::differ::{VdomNode, VdomState};
use crate::extractor::ExtractionPipeline;
use crate::input::InputDispatcher;
use crate::session::SessionManager;

use futures_util::StreamExt;
use proto_definitions::browser::browsing_service_server::BrowsingService;
use proto_definitions::browser::*;
use std::pin::Pin;
use std::sync::Arc;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tokio_stream::Stream;
use tonic::{Request, Response, Status, Streaming};

fn generate_session_id() -> String {
    format!("{:016x}{:016x}", fastrand::u64(..), fastrand::u64(..))
}

/// Build a simple VdomNode from markdown text for diffing
fn markdown_to_vdom(markdown: &str) -> VdomNode {
    let children: Vec<VdomNode> = markdown
        .lines()
        .map(|line| VdomNode::Text(line.to_string()))
        .collect();
    VdomNode::Element {
        tag: "body".to_string(),
        children,
    }
}

pub struct BrowserBackend {
    browser_pool: Arc<BrowserPool>,
    session_manager: SessionManager,
}

impl BrowserBackend {
    pub async fn new(
        session_manager: SessionManager,
    ) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let browser_pool = Arc::new(BrowserPool::new().await?);
        Ok(Self {
            browser_pool,
            session_manager,
        })
    }
}

#[tonic::async_trait]
impl BrowsingService for BrowserBackend {
    type SessionStream =
        Pin<Box<dyn Stream<Item = Result<PageUpdate, Status>> + Send + 'static>>;

    async fn session(
        &self,
        request: Request<Streaming<Interaction>>,
    ) -> Result<Response<Self::SessionStream>, Status> {
        let mut in_stream = request.into_inner();
        let (tx, rx) = mpsc::channel(128);

        let browser_pool = Arc::clone(&self.browser_pool);
        let session_manager = self.session_manager.clone();

        tokio::spawn(async move {
            let mut session_id: Option<String> = None;
            let mut vdom_state = VdomState::new();
            let mut compressor = PatchCompressor::new();

            while let Some(result) = in_stream.next().await {
                let interaction = match result {
                    Ok(i) => i,
                    Err(e) => {
                        tracing::error!("Stream error: {}", e);
                        break;
                    }
                };

                let r#type = match interaction.r#type {
                    Some(t) => t,
                    None => continue,
                };

                match r#type {
                    interaction::Type::Navigate(nav) => {
                        handle_navigate(
                            &nav,
                            &browser_pool,
                            &session_manager,
                            &tx,
                            &mut session_id,
                            &mut vdom_state,
                            &mut compressor,
                        )
                        .await;
                    }

                    interaction::Type::Click(click) => {
                        handle_click(
                            &click,
                            &browser_pool,
                            &session_manager,
                            &tx,
                            &session_id,
                            &mut vdom_state,
                            &mut compressor,
                        )
                        .await;
                    }

                    interaction::Type::Input(input_req) => {
                        handle_input(&input_req, &session_manager, &session_id).await;
                    }
                }
            }

            // Cleanup session on stream close
            if let Some(sid) = session_id {
                tracing::info!("Cleaning up session: {}", sid);
                session_manager.remove_session(&sid).await;
            }
        });

        Ok(Response::new(Box::pin(ReceiverStream::new(rx))))
    }
}

/// Handle NavigateRequest: create session if needed, navigate, extract, send full page
async fn handle_navigate(
    nav: &NavigateRequest,
    browser_pool: &BrowserPool,
    session_manager: &SessionManager,
    tx: &mpsc::Sender<Result<PageUpdate, Status>>,
    session_id: &mut Option<String>,
    vdom_state: &mut VdomState,
    compressor: &mut PatchCompressor,
) {
    let sid = if nav.session_id.is_empty() {
        let new_id = generate_session_id();
        match browser_pool.new_page().await {
            Ok(page) => {
                session_manager.create_session(new_id.clone(), page).await;
                *session_id = Some(new_id.clone());
                new_id
            }
            Err(e) => {
                let _ = tx
                    .send(Ok(PageUpdate {
                        content: Some(page_update::Content::Status(StatusMessage {
                            message: format!("Failed to create session: {}", e),
                            level: StatusLevel::Error.into(),
                        })),
                    }))
                    .await;
                return;
            }
        }
    } else {
        nav.session_id.clone()
    };

    let _ = tx
        .send(Ok(PageUpdate {
            content: Some(page_update::Content::Status(StatusMessage {
                message: format!("Loading {}...", nav.url),
                level: StatusLevel::Info.into(),
            })),
        }))
        .await;

    if let Some(page) = session_manager.get_page(&sid).await {
        match BrowserPool::navigate(&page, &nav.url).await {
            Ok(html) => {
                session_manager.set_current_url(&sid, nav.url.clone()).await;
                let result = ExtractionPipeline::process(&html, &nav.url);

                session_manager
                    .set_link_map(&sid, result.link_map.clone())
                    .await;

                // Update VDOM state for future diffing
                let new_vdom = markdown_to_vdom(&result.markdown);
                let patches = vdom_state.update(new_vdom);
                if !patches.is_empty() {
                    let patch_bytes = VdomState::serialize_patches(&patches);
                    let compressed = compressor.compress(&patch_bytes);
                    tracing::debug!(
                        "VDOM diff: {} patches, {} bytes compressed",
                        patches.len(),
                        compressed.len()
                    );
                }

                let _ = tx
                    .send(Ok(PageUpdate {
                        content: Some(page_update::Content::FullPage(PreRenderedPage {
                            markdown: result.markdown,
                            link_map: result.link_map,
                            title: result.title,
                            url: nav.url.clone(),
                        })),
                    }))
                    .await;
            }
            Err(e) => {
                let _ = tx
                    .send(Ok(PageUpdate {
                        content: Some(page_update::Content::Status(StatusMessage {
                            message: format!("Navigation failed: {}", e),
                            level: StatusLevel::Error.into(),
                        })),
                    }))
                    .await;
            }
        }
    }
}

/// Handle ClickRequest: resolve link_id to URL, navigate, extract, send full page
async fn handle_click(
    click: &ClickRequest,
    _browser_pool: &BrowserPool,
    session_manager: &SessionManager,
    tx: &mpsc::Sender<Result<PageUpdate, Status>>,
    session_id: &Option<String>,
    vdom_state: &mut VdomState,
    compressor: &mut PatchCompressor,
) {
    let sid = match session_id {
        Some(s) => s,
        None => return,
    };

    let resolved_url = session_manager.resolve_link(sid, click.link_id).await;

    if let Some(url) = resolved_url {
        let _ = tx
            .send(Ok(PageUpdate {
                content: Some(page_update::Content::Status(StatusMessage {
                    message: format!("Loading {}...", url),
                    level: StatusLevel::Info.into(),
                })),
            }))
            .await;

        if let Some(page) = session_manager.get_page(sid).await {
            match BrowserPool::navigate(&page, &url).await {
                Ok(html) => {
                    session_manager.set_current_url(sid, url.clone()).await;
                    let result = ExtractionPipeline::process(&html, &url);

                    session_manager
                        .set_link_map(sid, result.link_map.clone())
                        .await;

                    let new_vdom = markdown_to_vdom(&result.markdown);
                    let patches = vdom_state.update(new_vdom);
                    if !patches.is_empty() {
                        let patch_bytes = VdomState::serialize_patches(&patches);
                        let compressed = compressor.compress(&patch_bytes);
                        tracing::debug!(
                            "Click VDOM diff: {} patches, {} bytes compressed",
                            patches.len(),
                            compressed.len()
                        );
                    }

                    let _ = tx
                        .send(Ok(PageUpdate {
                            content: Some(page_update::Content::FullPage(PreRenderedPage {
                                markdown: result.markdown,
                                link_map: result.link_map,
                                title: result.title,
                                url,
                            })),
                        }))
                        .await;
                }
                Err(e) => {
                    let _ = tx
                        .send(Ok(PageUpdate {
                            content: Some(page_update::Content::Status(StatusMessage {
                                message: format!("Navigation failed: {}", e),
                                level: StatusLevel::Error.into(),
                            })),
                        }))
                        .await;
                }
            }
        }
    } else {
        let _ = tx
            .send(Ok(PageUpdate {
                content: Some(page_update::Content::Status(StatusMessage {
                    message: format!("Unknown link ID: {}", click.link_id),
                    level: StatusLevel::Warning.into(),
                })),
            }))
            .await;
    }
}

/// Handle InputRequest: dispatch keystrokes to the browser via CDP
async fn handle_input(
    input_req: &InputRequest,
    session_manager: &SessionManager,
    session_id: &Option<String>,
) {
    let sid = match session_id {
        Some(s) => s,
        None => return,
    };

    if let Some(page) = session_manager.get_page(sid).await {
        if input_req.is_submit {
            let _ = InputDispatcher::submit(&page).await;
        } else {
            let _ = InputDispatcher::type_text(&page, &input_req.text).await;
        }
    }
}
