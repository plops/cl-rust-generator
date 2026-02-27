use proto_definitions::browser::browsing_service_client::BrowsingServiceClient;
use proto_definitions::browser::*;
use futures_util::StreamExt;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let channel = tonic::transport::Channel::from_static("http://[::1]:50051")
        .connect()
        .await?;
    eprintln!("Connected to server");

    let mut client = BrowsingServiceClient::new(channel);
    let (tx, rx) = mpsc::channel::<Interaction>(32);
    let stream = ReceiverStream::new(rx);
    let response = client.session(stream).await?;
    let mut update_stream = response.into_inner();
    eprintln!("Session established");

    // Send navigate
    tx.send(Interaction {
        r#type: Some(interaction::Type::Navigate(NavigateRequest {
            url: "https://news.ycombinator.com".to_string(),
            session_id: String::new(),
        })),
    }).await?;
    eprintln!("Navigate request sent");

    // Wait for responses
    let mut count = 0;
    while let Some(result) = update_stream.next().await {
        count += 1;
        match result {
            Ok(update) => {
                match update.content {
                    Some(page_update::Content::FullPage(page)) => {
                        eprintln!("Got FullPage: title='{}', url='{}', markdown_len={}, links={}",
                            page.title, page.url, page.markdown.len(), page.link_map.len());
                        eprintln!("First 500 chars of markdown:");
                        eprintln!("{}", &page.markdown[..page.markdown.len().min(500)]);
                    }
                    Some(page_update::Content::Status(msg)) => {
                        eprintln!("Got Status: level={}, message='{}'", msg.level, msg.message);
                    }
                    Some(page_update::Content::Patch(delta)) => {
                        eprintln!("Got Patch: {} bytes", delta.compressed_patch.len());
                    }
                    None => {
                        eprintln!("Got empty update");
                    }
                }
            }
            Err(e) => {
                eprintln!("Stream error: {}", e);
                break;
            }
        }
        if count > 10 { break; }
    }
    eprintln!("Done, received {} updates", count);
    Ok(())
}
