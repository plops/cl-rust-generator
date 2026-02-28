//! Integration tests for the cloud-proxy-server.
//!
//! These tests spin up a real server with headless Chrome, connect a gRPC client,
//! and exercise the full pipeline: navigate, extract, stream, click, input.
//!
//! Requirements: google-chrome-stable or chromium in $PATH.
//! Run with: cargo test -p cloud-proxy-server --test integration

use cloud_proxy_server::service::BrowserBackend;
use cloud_proxy_server::session::SessionManager;

use futures_util::StreamExt;
use proto_definitions::browser::browsing_service_client::BrowsingServiceClient;
use proto_definitions::browser::browsing_service_server::BrowsingServiceServer;
use proto_definitions::browser::*;
use std::net::SocketAddr;
use std::time::Duration;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;
use tonic::transport::{Channel, Server};

/// Start the server on a random port, return the address.
async fn start_server() -> SocketAddr {
    let listener = tokio::net::TcpListener::bind("[::1]:0").await.unwrap();
    let addr = listener.local_addr().unwrap();

    tokio::spawn(async move {
        let session_manager = SessionManager::new();
        let reaper = session_manager.clone();
        tokio::spawn(async move { reaper.reap_idle_sessions_loop().await });

        let flags = cloud_proxy_server::service::ResourceFlags::default();
        let backend = BrowserBackend::new(session_manager, true, flags, None, Vec::new())
            .await
            .unwrap();
        let svc = BrowsingServiceServer::new(backend);

        Server::builder()
            .add_service(svc)
            .serve_with_incoming(tokio_stream::wrappers::TcpListenerStream::new(listener))
            .await
            .unwrap();
    });

    // Give the server a moment to bind
    tokio::time::sleep(Duration::from_millis(200)).await;
    addr
}

/// Open a bidirectional session, returning the sender and response stream.
async fn open_session(
    addr: SocketAddr,
) -> (
    mpsc::Sender<Interaction>,
    tonic::Streaming<PageUpdate>,
) {
    let channel = Channel::from_shared(format!("http://{}", addr))
        .unwrap()
        .connect()
        .await
        .unwrap();

    let mut client = BrowsingServiceClient::new(channel);
    let (tx, rx) = mpsc::channel::<Interaction>(32);
    let stream = ReceiverStream::new(rx);
    let response = client.session(stream).await.unwrap();
    (tx, response.into_inner())
}

/// Collect page updates until we get a FullPage or timeout.
async fn wait_for_full_page(
    stream: &mut tonic::Streaming<PageUpdate>,
    timeout: Duration,
) -> Option<PreRenderedPage> {
    let deadline = tokio::time::Instant::now() + timeout;
    loop {
        tokio::select! {
            update = stream.next() => {
                match update {
                    Some(Ok(pu)) => {
                        if let Some(page_update::Content::FullPage(page)) = pu.content {
                            return Some(page);
                        }
                        // Status messages are expected, keep waiting
                    }
                    _ => return None,
                }
            }
            _ = tokio::time::sleep_until(deadline) => {
                return None;
            }
        }
    }
}

/// Collect all updates until timeout, returning them.
async fn collect_updates(
    stream: &mut tonic::Streaming<PageUpdate>,
    timeout: Duration,
) -> Vec<PageUpdate> {
    let mut updates = Vec::new();
    let deadline = tokio::time::Instant::now() + timeout;
    loop {
        tokio::select! {
            update = stream.next() => {
                match update {
                    Some(Ok(pu)) => updates.push(pu),
                    _ => break,
                }
            }
            _ = tokio::time::sleep_until(deadline) => break,
        }
    }
    updates
}

// =========================================================================
// Tests
// =========================================================================

/// Test 1: Navigate to a simple page and receive markdown content.
#[tokio::test]
async fn test_navigate_returns_full_page() {
    let addr = start_server().await;
    let (tx, mut stream) = open_session(addr).await;

    // Navigate to example.com — a simple, stable page
    tx.send(Interaction {
        r#type: Some(interaction::Type::Navigate(NavigateRequest {
            url: "https://example.com".to_string(),
            session_id: String::new(),
        })),
    })
    .await
    .unwrap();

    let page = wait_for_full_page(&mut stream, Duration::from_secs(30))
        .await
        .expect("Should receive a FullPage update");

    // example.com has a known title and content
    assert!(!page.markdown.is_empty(), "Markdown should not be empty");
    assert!(!page.title.is_empty(), "Title should not be empty");
    assert_eq!(page.url, "https://example.com");
    // example.com contains "Example Domain" in its content
    assert!(
        page.markdown.contains("Example Domain")
            || page.title.contains("Example Domain"),
        "Page should contain 'Example Domain', got title='{}', markdown starts with '{}'",
        page.title,
        &page.markdown[..page.markdown.len().min(200)]
    );

    println!("  title: {}", page.title);
    println!("  markdown length: {} bytes", page.markdown.len());
    println!("  link_map entries: {}", page.link_map.len());
}

/// Test 2: Navigate to a page with links and verify link_map is populated.
#[tokio::test]
async fn test_link_map_populated() {
    let addr = start_server().await;
    let (tx, mut stream) = open_session(addr).await;

    // Wikipedia has lots of links
    tx.send(Interaction {
        r#type: Some(interaction::Type::Navigate(NavigateRequest {
            url: "https://www.iana.org/domains/reserved".to_string(),
            session_id: String::new(),
        })),
    })
    .await
    .unwrap();

    let page = wait_for_full_page(&mut stream, Duration::from_secs(60))
        .await
        .expect("Should receive a FullPage update");

    assert!(!page.link_map.is_empty(), "Page should have links");
    assert!(
        page.link_map.len() >= 1,
        "Expected links, got {}",
        page.link_map.len()
    );

    // All link_map values should be valid URLs
    for (_id, url) in &page.link_map {
        assert!(
            url.starts_with("http://") || url.starts_with("https://"),
            "Link URL should be absolute: {}",
            url
        );
    }

    println!("  title: {}", page.title);
    println!("  links: {}", page.link_map.len());
    println!("  markdown: {} bytes", page.markdown.len());
}

/// Test 3: Status messages arrive before the full page.
#[tokio::test]
async fn test_status_messages_before_page() {
    let addr = start_server().await;
    let (tx, mut stream) = open_session(addr).await;

    tx.send(Interaction {
        r#type: Some(interaction::Type::Navigate(NavigateRequest {
            url: "https://example.com".to_string(),
            session_id: String::new(),
        })),
    })
    .await
    .unwrap();

    let updates = collect_updates(&mut stream, Duration::from_secs(30)).await;

    // We should get at least one status ("Loading...") and one FullPage
    let statuses: Vec<_> = updates
        .iter()
        .filter(|u| matches!(&u.content, Some(page_update::Content::Status(_))))
        .collect();
    let pages: Vec<_> = updates
        .iter()
        .filter(|u| matches!(&u.content, Some(page_update::Content::FullPage(_))))
        .collect();

    assert!(
        !statuses.is_empty(),
        "Should receive at least one status message"
    );
    assert!(
        !pages.is_empty(),
        "Should receive at least one full page"
    );

    // The first status should mention "Loading"
    if let Some(page_update::Content::Status(s)) = &statuses[0].content {
        assert!(
            s.message.contains("Loading"),
            "First status should mention Loading, got: {}",
            s.message
        );
    }

    println!("  statuses: {}", statuses.len());
    println!("  pages: {}", pages.len());
    println!("  total updates: {}", updates.len());
}

/// Test 4: Sequential navigations work within the same session.
#[tokio::test]
async fn test_sequential_navigation() {
    let addr = start_server().await;
    let (tx, mut stream) = open_session(addr).await;

    // First navigation
    tx.send(Interaction {
        r#type: Some(interaction::Type::Navigate(NavigateRequest {
            url: "https://example.com".to_string(),
            session_id: String::new(),
        })),
    })
    .await
    .unwrap();

    let page1 = wait_for_full_page(&mut stream, Duration::from_secs(30))
        .await
        .expect("First navigation should return a page");

    // Second navigation
    tx.send(Interaction {
        r#type: Some(interaction::Type::Navigate(NavigateRequest {
            url: "https://www.iana.org/domains/reserved".to_string(),
            session_id: String::new(),
        })),
    })
    .await
    .unwrap();

    let page2 = wait_for_full_page(&mut stream, Duration::from_secs(30))
        .await
        .expect("Second navigation should return a page");

    // Pages should be different
    assert_ne!(
        page1.url, page2.url,
        "URLs should differ between navigations"
    );
    assert_ne!(
        page1.markdown, page2.markdown,
        "Content should differ between navigations"
    );

    println!("  page1: {} ({} bytes)", page1.title, page1.markdown.len());
    println!("  page2: {} ({} bytes)", page2.title, page2.markdown.len());
}

/// Test 5: Input dispatch doesn't crash the server.
#[tokio::test]
async fn test_input_dispatch() {
    let addr = start_server().await;
    let (tx, mut stream) = open_session(addr).await;

    // Navigate first to establish a page
    tx.send(Interaction {
        r#type: Some(interaction::Type::Navigate(NavigateRequest {
            url: "https://example.com".to_string(),
            session_id: String::new(),
        })),
    })
    .await
    .unwrap();

    let _page = wait_for_full_page(&mut stream, Duration::from_secs(30))
        .await
        .expect("Should get a page first");

    // Send text input — should not crash
    tx.send(Interaction {
        r#type: Some(interaction::Type::Input(InputRequest {
            text: "hello".to_string(),
            session_id: String::new(),
            is_submit: false,
        })),
    })
    .await
    .unwrap();

    // Send submit — should not crash
    tx.send(Interaction {
        r#type: Some(interaction::Type::Input(InputRequest {
            text: String::new(),
            session_id: String::new(),
            is_submit: true,
        })),
    })
    .await
    .unwrap();

    // Give the server a moment to process
    tokio::time::sleep(Duration::from_millis(500)).await;

    // If we can still navigate, the server is alive
    tx.send(Interaction {
        r#type: Some(interaction::Type::Navigate(NavigateRequest {
            url: "https://example.com".to_string(),
            session_id: String::new(),
        })),
    })
    .await
    .unwrap();

    let page = wait_for_full_page(&mut stream, Duration::from_secs(30))
        .await
        .expect("Server should still be alive after input dispatch");

    assert!(!page.markdown.is_empty());
    println!("  Server survived input dispatch, page: {} bytes", page.markdown.len());
}

/// Test 6: Click with invalid link_id returns a warning status.
#[tokio::test]
async fn test_click_invalid_link() {
    let addr = start_server().await;
    let (tx, mut stream) = open_session(addr).await;

    // Navigate first
    tx.send(Interaction {
        r#type: Some(interaction::Type::Navigate(NavigateRequest {
            url: "https://example.com".to_string(),
            session_id: String::new(),
        })),
    })
    .await
    .unwrap();

    let _page = wait_for_full_page(&mut stream, Duration::from_secs(30))
        .await
        .expect("Should get a page");

    // Click a non-existent link
    tx.send(Interaction {
        r#type: Some(interaction::Type::Click(ClickRequest {
            link_id: 99999,
            session_id: String::new(),
        })),
    })
    .await
    .unwrap();

    // Should get a warning status about unknown link
    let updates = collect_updates(&mut stream, Duration::from_secs(5)).await;
    let has_warning = updates.iter().any(|u| {
        if let Some(page_update::Content::Status(s)) = &u.content {
            s.message.contains("Unknown link ID")
        } else {
            false
        }
    });

    assert!(has_warning, "Should receive 'Unknown link ID' warning");
    println!("  Correctly received unknown link warning");
}

/// Test 7: Extraction pipeline handles data URIs without panicking.
#[tokio::test]
async fn test_data_uri_no_panic() {
    // Unit-level test of the extractor with a data URI that previously caused a panic
    let html = r#"<html><head><title>Test</title></head><body>
        <img src="data:image/svg+xml;base64,PHN2ZyB2aWV3Qm94PSIwIDAgMjQgMjQiPjwvc3ZnPg==">
        <a href="https://example.com">Link</a>
        <p>Hello world</p>
    </body></html>"#;

    let result = cloud_proxy_server::extractor::ExtractionPipeline::process(html, "https://test.com");
    assert!(!result.markdown.is_empty(), "Should produce markdown");
    assert!(
        result.markdown.contains("Hello world") || result.markdown.contains("Link"),
        "Should contain page text"
    );
    println!("  Extracted {} bytes, {} links", result.markdown.len(), result.link_map.len());
}

/// Test 8: Compressor round-trip works.
#[tokio::test]
async fn test_compressor_roundtrip() {
    let mut compressor = cloud_proxy_server::compressor::PatchCompressor::new();
    let mut decompressor = cloud_proxy_server::compressor::PatchDecompressor::new();

    let original = b"Hello, this is a test payload for zstd compression round-trip.";
    let compressed = compressor.compress(original);
    let decompressed = decompressor.decompress(&compressed, 1024).unwrap();

    assert_eq!(&decompressed, original);
    println!(
        "  original: {} bytes, compressed: {} bytes",
        original.len(),
        compressed.len()
    );
}

/// Test 9: VDOM differ produces correct patches.
#[tokio::test]
async fn test_vdom_diff() {
    use cloud_proxy_server::differ::{VdomNode, VdomState};

    let mut state = VdomState::new();

    // First update — should produce a ReplaceNode (initial)
    let tree1 = VdomNode::Element {
        tag: "body".to_string(),
        children: vec![
            VdomNode::Text("Line 1".to_string()),
            VdomNode::Text("Line 2".to_string()),
        ],
    };
    let patches1 = state.update(tree1);
    assert!(!patches1.is_empty(), "First update should produce patches");

    // Second update — change one line
    let tree2 = VdomNode::Element {
        tag: "body".to_string(),
        children: vec![
            VdomNode::Text("Line 1".to_string()),
            VdomNode::Text("Line 2 MODIFIED".to_string()),
        ],
    };
    let patches2 = state.update(tree2);
    assert!(!patches2.is_empty(), "Modified tree should produce patches");

    // Third update — identical, no patches
    let tree3 = VdomNode::Element {
        tag: "body".to_string(),
        children: vec![
            VdomNode::Text("Line 1".to_string()),
            VdomNode::Text("Line 2 MODIFIED".to_string()),
        ],
    };
    let patches3 = state.update(tree3);
    assert!(patches3.is_empty(), "Identical tree should produce no patches");

    println!("  initial patches: {}, modified: {}, identical: {}", patches1.len(), patches2.len(), patches3.len());
}

/// Test 10: Priority queue respects priority ordering and bandwidth budget.
#[tokio::test]
async fn test_priority_queue_ordering() {
    use cloud_proxy_server::throttle::{Priority, PriorityQueue};

    let mut queue = PriorityQueue::new();

    // Enqueue in reverse priority order
    queue.enqueue(Priority::Metadata, b"meta".to_vec());
    queue.enqueue(Priority::Patch, b"patch".to_vec());
    queue.enqueue(Priority::Content, b"content".to_vec());
    queue.enqueue(Priority::Status, b"status".to_vec());

    // Should dequeue in priority order: Status, Content, Patch, Metadata
    let (p1, d1) = queue.dequeue().unwrap();
    assert_eq!(p1, Priority::Status);
    assert_eq!(d1, b"status");

    let (p2, d2) = queue.dequeue().unwrap();
    assert_eq!(p2, Priority::Content);
    assert_eq!(d2, b"content");

    let (p3, d3) = queue.dequeue().unwrap();
    assert_eq!(p3, Priority::Patch);
    assert_eq!(d3, b"patch");

    let (p4, d4) = queue.dequeue().unwrap();
    assert_eq!(p4, Priority::Metadata);
    assert_eq!(d4, b"meta");

    assert!(queue.dequeue().is_none());
    println!("  Priority ordering verified: Status > Content > Patch > Metadata");
}
