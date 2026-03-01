use anyhow::{Result, anyhow};
use image::{DynamicImage, ImageFormat, RgbaImage};

pub struct TestImageData {
    pub width: u32,
    pub height: u32,
    pub rgb_data: Vec<u8>,  // RGB8 format (3 bytes per pixel)
    pub rgba_data: Vec<u8>, // RGBA8 format (4 bytes per pixel)
}

pub fn load_test_image(path: &std::path::Path) -> Result<TestImageData> {
    println!("[Client] Loading test image from: {:?}", path);
    
    // Load image using image crate
    let img = image::open(path)
        .map_err(|e| anyhow!("Failed to load image {}: {}", path.display(), e))?;
    
    // Convert to RGBA8 format
    let rgba_img = img.to_rgba8();
    let (width, height) = rgba_img.dimensions();
    
    println!("[Client] Loaded image: {}x{}", width, height);
    
    // Extract RGBA data
    let rgba_data = rgba_img.into_raw();
    
    // Convert to RGB data (remove alpha channel)
    let mut rgb_data = Vec::with_capacity((width * height * 3) as usize);
    for chunk in rgba_data.chunks_exact(4) {
        rgb_data.extend_from_slice(&[chunk[0], chunk[1], chunk[2]]); // R, G, B
    }
    
    println!("[Client] Converted to RGB: {} bytes", rgb_data.len());
    
    Ok(TestImageData {
        width,
        height,
        rgb_data,
        rgba_data,
    })
}

pub fn save_image_as_png(rgba_data: &[u8], width: u32, height: u32, path: &std::path::Path) -> Result<()> {
    let img = RgbaImage::from_raw(width, height, rgba_data.to_vec())
        .ok_or_else(|| anyhow!("Failed to create RGBA image buffer"))?;
    
    let dynamic_img = DynamicImage::ImageRgba8(img);
    dynamic_img.save_with_format(path, ImageFormat::Png)
        .map_err(|e| anyhow!("Failed to save PNG {}: {}", path.display(), e))?;
    
    println!("[Client] Saved PNG: {}", path.display());
    Ok(())
}
