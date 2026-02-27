use crate::browser::BrowserPool;
use crate::compressor::PatchCompressor;
use crate::differ::{VdomNode, VdomState};
use crate::extractor::ExtractionPipeline;
use crate::input::InputDispatcher;
use crate::session::SessionManager;
use crate::throttle::{Debouncer, Priority, PriorityQueue};

use futures_util::StreamExt;
use proto_definitions::browser::browsing_service_server::BrowsingService;
use proto_definitions::browser::*;
use std::pin::Pin;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tokio_stream::Stream;
use tonic::{Request, Response, Status, Streaming};

#[derive(Clone, Debug, Default)]
pub struct ResourceFlags {
    pub load_images: bool,
    pub load_media: bool,
    pub load_css: bool,
    pub load_fonts: bool,
    pub load_all: bool,
}

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
        headless: bool,
        flags: ResourceFlags,
    ) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let browser_pool = Arc::new(BrowserPool::new(headless, flags).await?);
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
            let mut priority_queue = PriorityQueue::new();
            let mut debouncer = Debouncer::new();

            // Channel for DOM mutation notifications from the browser watcher
            let (dom_tx, mut dom_rx) = mpsc::channel::<()>(16);

            // Drain interval: flush the priority queue at ~20Hz
            let mut drain_interval = tokio::time::interval(Duration::from_millis(50));

            loop {
                tokio::select! {
                    biased;

                    // 1. Incoming client interactions
                    result = in_stream.next() => {
                        let interaction = match result {
                            Some(Ok(i)) => i,
                            Some(Err(e)) => {
                                tracing::error!("Stream error: {}", e);
                                break;
                            }
                            None => break,
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
                                    &mut priority_queue,
                                    &mut session_id,
                                    &mut vdom_state,
                                    &mut compressor,
                                    &dom_tx,
                                )
                                .await;
                            }
                            interaction::Type::Click(click) => {
                                handle_click(
                                    &click,
                                    &browser_pool,
                                    &session_manager,
                                    &mut priority_queue,
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

                        // Eagerly drain after handling an interaction
                        drain_priority_queue(&mut priority_queue, &tx).await;
                    }

                    // 2. DOM mutation events from the browser watcher
                    _ = dom_rx.recv() => {
                        // Feed through debouncer to coalesce rapid mutations
                        if let Some(_coalesced) = debouncer.push(vec![1]) {
                            handle_dom_mutation(
                                &session_manager,
                                &mut priority_queue,
                                &session_id,
                                &mut vdom_state,
                                &mut compressor,
                            )
                            .await;
                        }
                    }

                    // 3. Periodic drain of the priority queue
                    _ = drain_interval.tick() => {
                        drain_priority_queue(&mut priority_queue, &tx).await;
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

// ---------------------------------------------------------------------------
// Priority queue encoding helpers
// We stash PageUpdate data as tagged byte vectors in the priority queue.
// Tag byte: 0=Status, 1=FullPage, 2=Patch
// ---------------------------------------------------------------------------

fn encode_status(message: &str, level: StatusLevel) -> Vec<u8> {
    let mut data = vec![0u8];
    data.push(level as u8);
    data.extend_from_slice(message.as_bytes());
    data
}

fn encode_full_page(
    markdown: &str,
    link_map: &std::collections::HashMap<u32, String>,
    title: &str,
    url: &str,
) -> Vec<u8> {
    let mut data = vec![1u8];
    data.extend_from_slice(markdown.as_bytes());
    data.push(0);
    for (id, link_url) in link_map {
        data.extend_from_slice(format!("{}:{}\n", id, link_url).as_bytes());
    }
    data.push(0);
    data.extend_from_slice(title.as_bytes());
    data.push(0);
    data.extend_from_slice(url.as_bytes());
    data
}

fn encode_patch(compressed: &[u8]) -> Vec<u8> {
    let mut data = vec![2u8];
    data.extend_from_slice(compressed);
    data
}

/// Drain all sendable messages from the priority queue into the gRPC stream
async fn drain_priority_queue(
    queue: &mut PriorityQueue,
    tx: &mpsc::Sender<Result<PageUpdate, Status>>,
) {
    while let Some((_priority, data)) = queue.dequeue() {
        if data.is_empty() {
            continue;
        }
        let tag = data[0];
        let payload = &data[1..];
        let update = match tag {
            0 => {
                if payload.is_empty() { continue; }
                let level = payload[0] as i32;
                let message = String::from_utf8_lossy(&payload[1..]).to_string();
                PageUpdate {
                    content: Some(page_update::Content::Status(StatusMessage {
                        message,
                        level,
                    })),
                }
            }
            1 => {
                let parts: Vec<&[u8]> = payload.splitn(4, |&b| b == 0).collect();
                if parts.len() < 4 { continue; }
                let markdown = String::from_utf8_lossy(parts[0]).to_string();
                let link_map_str = String::from_utf8_lossy(parts[1]);
                let title = String::from_utf8_lossy(parts[2]).to_string();
                let url = String::from_utf8_lossy(parts[3]).to_string();
                let mut link_map = std::collections::HashMap::new();
                for line in link_map_str.lines() {
                    if let Some((k, v)) = line.split_once(':') {
                        if let Ok(id) = k.parse::<u32>() {
                            link_map.insert(id, v.to_string());
                        }
                    }
                }
                PageUpdate {
                    content: Some(page_update::Content::FullPage(PreRenderedPage {
                        markdown,
                        link_map,
                        title,
                        url,
                    })),
                }
            }
            2 => {
                PageUpdate {
                    content: Some(page_update::Content::Patch(DomDelta {
                        compressed_patch: payload.to_vec(),
                    })),
                }
            }
            _ => continue,
        };
        if tx.send(Ok(update)).await.is_err() {
            break;
        }
    }
}

// ---------------------------------------------------------------------------
// Interaction handlers
// ---------------------------------------------------------------------------

async fn handle_navigate(
    nav: &NavigateRequest,
    browser_pool: &BrowserPool,
    session_manager: &SessionManager,
    queue: &mut PriorityQueue,
    session_id: &mut Option<String>,
    vdom_state: &mut VdomState,
    compressor: &mut PatchCompressor,
    dom_tx: &mpsc::Sender<()>,
) {
    let sid = if nav.session_id.is_empty() {
        let new_id = generate_session_id();
        match browser_pool.new_page_with_dom_watcher(dom_tx.clone()).await {
            Ok(page) => {
                session_manager.create_session(new_id.clone(), page).await;
                *session_id = Some(new_id.clone());
                new_id
            }
            Err(e) => {
                queue.enqueue(
                    Priority::Status,
                    encode_status(&format!("Failed to create session: {}", e), StatusLevel::Error),
                );
                return;
            }
        }
    } else {
        nav.session_id.clone()
    };

    queue.enqueue(
        Priority::Status,
        encode_status(&format!("Loading {}...", nav.url), StatusLevel::Info),
    );

    if let Some(page) = session_manager.get_page(&sid).await {
        match BrowserPool::navigate(&page, &nav.url).await {
            Ok(html) => {
                session_manager.set_current_url(&sid, nav.url.clone()).await;
                let result = ExtractionPipeline::process(&html, &nav.url);
                session_manager.set_link_map(&sid, result.link_map.clone()).await;

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

                queue.enqueue(
                    Priority::Content,
                    encode_full_page(&result.markdown, &result.link_map, &result.title, &nav.url),
                );
            }
            Err(e) => {
                queue.enqueue(
                    Priority::Status,
                    encode_status(&format!("Navigation failed: {}", e), StatusLevel::Error),
                );
            }
        }
    }
}

async fn handle_click(
    click: &ClickRequest,
    _browser_pool: &BrowserPool,
    session_manager: &SessionManager,
    queue: &mut PriorityQueue,
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
        queue.enqueue(
            Priority::Status,
            encode_status(&format!("Loading {}...", url), StatusLevel::Info),
        );

        if let Some(page) = session_manager.get_page(sid).await {
            match BrowserPool::navigate(&page, &url).await {
                Ok(html) => {
                    session_manager.set_current_url(sid, url.clone()).await;
                    let result = ExtractionPipeline::process(&html, &url);
                    session_manager.set_link_map(sid, result.link_map.clone()).await;

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

                    queue.enqueue(
                        Priority::Content,
                        encode_full_page(&result.markdown, &result.link_map, &result.title, &url),
                    );
                }
                Err(e) => {
                    queue.enqueue(
                        Priority::Status,
                        encode_status(&format!("Navigation failed: {}", e), StatusLevel::Error),
                    );
                }
            }
        }
    } else {
        queue.enqueue(
            Priority::Status,
            encode_status(&format!("Unknown link ID: {}", click.link_id), StatusLevel::Warning),
        );
    }
}

/// Handle DOM mutation: re-extract content, diff, send patch if small or full page if large
async fn handle_dom_mutation(
    session_manager: &SessionManager,
    queue: &mut PriorityQueue,
    session_id: &Option<String>,
    vdom_state: &mut VdomState,
    compressor: &mut PatchCompressor,
) {
    let sid = match session_id {
        Some(s) => s,
        None => return,
    };

    if let Some(page) = session_manager.get_page(sid).await {
        let html = match page.content().await {
            Ok(h) => h,
            Err(e) => {
                tracing::warn!("Failed to get page content on DOM mutation: {}", e);
                return;
            }
        };

        let url = session_manager.get_current_url(sid).await.unwrap_or_default();
        let result = ExtractionPipeline::process(&html, &url);
        session_manager.set_link_map(sid, result.link_map.clone()).await;

        let new_vdom = markdown_to_vdom(&result.markdown);
        let patches = vdom_state.update(new_vdom);

        if patches.is_empty() {
            return;
        }

        let patch_bytes = VdomState::serialize_patches(&patches);
        let compressed = compressor.compress(&patch_bytes);

        tracing::debug!(
            "DOM mutation: {} patches, {} bytes compressed",
            patches.len(),
            compressed.len()
        );

        if compressed.len() < 2048 {
            queue.enqueue(Priority::Patch, encode_patch(&compressed));
        } else {
            queue.enqueue(
                Priority::Content,
                encode_full_page(&result.markdown, &result.link_map, &result.title, &url),
            );
        }
    }
}

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
