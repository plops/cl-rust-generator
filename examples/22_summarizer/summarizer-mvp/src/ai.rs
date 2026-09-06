#[derive(Debug, Clone, serde::Serialize)]
pub struct ReqPart {
    pub text: String,
}
#[derive(Debug, Clone, serde::Serialize)]
pub struct ReqContent {
    pub parts: Vec<ReqPart>,
}
#[derive(Debug, Clone, serde::Serialize)]
pub struct ReqGen {
    pub contents: Vec<ReqContent>,
}
#[derive(Debug, Clone, serde::Serialize)]
pub struct ReqEmbed {
    pub content: ReqContent,
}
#[derive(Debug, Clone, serde::Serialize)]
pub struct GenOutput {
    pub summary: String,
    pub in_tokens: i64,
    pub out_tokens: i64,
}
#[derive(Debug, Clone, serde::Deserialize)]
pub struct RespPart {
    pub text: Option<String>,
}
#[derive(Debug, Clone, serde::Deserialize)]
pub struct RespContent {
    pub parts: Option<Vec<RespPart>>,
}
#[derive(Debug, Clone, serde::Deserialize)]
pub struct RespCandidate {
    pub content: Option<RespContent>,
}
#[derive(Debug, Clone, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RespUsage {
    pub prompt_token_count: Option<u64>,
    pub candidates_token_count: Option<u64>,
}
#[derive(Debug, Clone, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RespGen {
    pub candidates: Option<Vec<RespCandidate>>,
    pub usage_metadata: Option<RespUsage>,
}
#[derive(Debug, Clone, serde::Deserialize)]
pub struct RespEmbedValues {
    pub values: Option<Vec<f32>>,
}
#[derive(Debug, Clone, serde::Deserialize)]
pub struct RespEmbed {
    pub embedding: Option<RespEmbedValues>,
}
pub fn cosine_similarity(a: &[f32], b: &[f32]) -> f32 {
    if a.is_empty() || b.is_empty() {
        return 0.0;
    }
    {
        let len = a.len().min(b.len());
        {
            let mut acc: f32 = 0.0;
            let mut na: f32 = 0.0;
            let mut nb: f32 = 0.0;
            for i in 0..len {
                acc += a[i] * b[i];
                na += a[i] * a[i];
                nb += b[i] * b[i]
            }
            if na == 0.0 || nb == 0.0 {
                return 0.0;
            }
            acc / (na.sqrt() * nb.sqrt())
        }
    }
}
pub fn embedding_to_bytes(embedding: &[f32]) -> Vec<u8> {
    embedding.iter().flat_map(|x| x.to_le_bytes()).collect()
}
pub fn bytes_to_embedding(bytes: &[u8]) -> Vec<f32> {
    bytes
        .as_chunks::<4>()
        .0
        .iter()
        .map(|c| f32::from_le_bytes(*c))
        .collect()
}
pub fn build_prompt(transcript: &str) -> String {
    format!(
        "Summarize the following transcript. Reply with facts, decisions and numbers first, then a short abstract: {}",
        transcript
    )
}
pub async fn generate_summary(
    client: &reqwest::Client,
    base_url: &str,
    api_key: &str,
    model: &str,
    transcript: &str,
) -> Result<GenOutput, String> {
    {
        let url = format!("{}/v1beta/models/{}:generateContent", base_url, model);
        let request = ReqGen {
            contents: vec![ReqContent {
                parts: vec![ReqPart {
                    text: build_prompt(transcript),
                }],
            }],
        };
        let response = client
            .post(url)
            .query(&[("key", api_key)])
            .json(&request)
            .send()
            .await
            .map_err(|e| e.to_string())?;
        if !response.status().is_success() {
            return Err(format!("gemini http error: {}", response.status()));
        }
        {
            let body: RespGen = response.json().await.map_err(|e| e.to_string())?;
            {
                let mut summary: String = "".to_string();
                let mut in_tokens: i64 = 0;
                let mut out_tokens: i64 = 0;
                if let Some(candidates) = &body.candidates {
                    {
                        let first = candidates.first();
                        if let Some(first) = first {
                            {
                                let content = &first.content;
                                if let Some(content) = content {
                                    {
                                        let parts = &content.parts;
                                        if let Some(parts) = parts {
                                            for p in parts {
                                                if let Some(text) = &p.text {
                                                    summary.push_str(text);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                if let Some(usage) = &body.usage_metadata {
                    if let Some(v) = &usage.prompt_token_count {
                        in_tokens = *v as i64;
                    }
                    if let Some(v) = &usage.candidates_token_count {
                        out_tokens = *v as i64;
                    }
                }
                Ok(GenOutput {
                    summary,
                    in_tokens,
                    out_tokens,
                })
            }
        }
    }
}
pub async fn embed_text(
    client: &reqwest::Client,
    base_url: &str,
    api_key: &str,
    text: &str,
) -> Result<Vec<f32>, String> {
    {
        let url = format!(
            "{}/v1beta/models/gemini-embedding-001:embedContent",
            base_url
        );
        let request = ReqEmbed {
            content: ReqContent {
                parts: vec![ReqPart {
                    text: text.to_string(),
                }],
            },
        };
        let response = client
            .post(url)
            .query(&[("key", api_key)])
            .json(&request)
            .send()
            .await
            .map_err(|e| e.to_string())?;
        if !response.status().is_success() {
            return Err(format!("gemini embed http error: {}", response.status()));
        }
        {
            let body: RespEmbed = response.json().await.map_err(|e| e.to_string())?;
            if let Some(embedding) = &body.embedding {
                if let Some(values) = &embedding.values {
                    Ok(values.clone())
                } else {
                    Err("no values in embedding".to_string())
                }
            } else {
                Err("no embedding in response".to_string())
            }
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn cosine_identical() {
        {
            let result = cosine_similarity(&[1.0, 2.0, 3.0], &[1.0, 2.0, 3.0]);
            assert!(result > 0.999990);
        }
    }
    #[test]
    fn cosine_orthogonal() {
        {
            let result = cosine_similarity(&[1.0, 0.0], &[0.0, 1.0]);
            assert!(result == 0.0);
        }
    }
    #[test]
    fn cosine_zero_vector() {
        {
            let result = cosine_similarity(&[0.0, 0.0], &[1.0, 2.0]);
            assert!(result == 0.0);
        }
    }
    #[test]
    fn cosine_empty() {
        {
            let result = cosine_similarity(&[], &[1.0]);
            assert!(result == 0.0);
        }
    }
    #[test]
    fn cosine_unequal_lengths() {
        {
            let result = cosine_similarity(&[1.0, 0.0, 0.0], &[1.0, 0.0]);
            assert!(result == 1.0);
        }
    }
    #[test]
    fn bytes_roundtrip() {
        {
            let original = vec![1.50, (-2.250), 0.0];
            let back = bytes_to_embedding(&embedding_to_bytes(&[1.50, (-2.250), 0.0]));
            assert!(back == original);
        }
    }
}
