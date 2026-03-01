use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComparisonResult {
    pub total_pixels: u64,
    pub different_pixels: u64,
    pub max_difference: f32,
    pub average_difference: f32,
    pub passed: bool,
    pub tolerance: f32,
}

pub fn compare_images(
    original_rgb: &[u8],
    decoded_rgba: &[u8],
    width: u32,
    height: u32,
    tolerance: f32,
) -> Result<ComparisonResult> {
    let total_pixels = (width * height) as u64;
    
    if original_rgb.len() != (total_pixels * 3) as usize {
        return Err(anyhow!("Original RGB data size mismatch: expected {}, got {}", 
                          total_pixels * 3, original_rgb.len()));
    }
    
    if decoded_rgba.len() != (total_pixels * 4) as usize {
        return Err(anyhow!("Decoded RGBA data size mismatch: expected {}, got {}", 
                          total_pixels * 4, decoded_rgba.len()));
    }
    
    let mut different_pixels = 0u64;
    let mut total_difference = 0.0f32;
    let mut max_difference = 0.0f32;
    
    // Compare each pixel
    for i in 0..total_pixels as usize {
        let orig_idx = i * 3;
        let dec_idx = i * 4;
        
        let orig_r = original_rgb[orig_idx] as f32;
        let orig_g = original_rgb[orig_idx + 1] as f32;
        let orig_b = original_rgb[orig_idx + 2] as f32;
        
        let dec_r = decoded_rgba[dec_idx] as f32;
        let dec_g = decoded_rgba[dec_idx + 1] as f32;
        let dec_b = decoded_rgba[dec_idx + 2] as f32;
        
        // Calculate Euclidean distance in RGB space
        let dr = orig_r - dec_r;
        let dg = orig_g - dec_g;
        let db = orig_b - dec_b;
        let difference = (dr * dr + dg * dg + db * db).sqrt();
        
        if difference > tolerance {
            different_pixels += 1;
        }
        
        total_difference += difference;
        max_difference = max_difference.max(difference);
    }
    
    let average_difference = total_difference / total_pixels as f32;
    let passed = different_pixels == 0 && max_difference <= tolerance;
    
    println!("[Client] Image comparison results:");
    println!("  Total pixels: {}", total_pixels);
    println!("  Different pixels: {}", different_pixels);
    println!("  Max difference: {:.2}", max_difference);
    println!("  Average difference: {:.4}", average_difference);
    println!("  Tolerance: {:.2}", tolerance);
    println!("  Passed: {}", passed);
    
    Ok(ComparisonResult {
        total_pixels,
        different_pixels,
        max_difference,
        average_difference,
        passed,
        tolerance,
    })
}
