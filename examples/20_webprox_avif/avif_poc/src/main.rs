use anyhow::{anyhow, Result};
use aom_decode::{avif::{Avif, Image}, Config};

fn main() -> Result<()> {
    println!("AVIF Decoder Proof of Concept using aom-decode");
    
    // Read the AVIF file
    let avif_data = std::fs::read("../screenshot.avif")?;
    println!("Read {} bytes from AVIF file", avif_data.len());
    
    // Configure decoder
    let config = Config {
        threads: std::thread::available_parallelism()?.get(),
    };
    
    // Decode AVIF using high-level API
    let mut avif = Avif::decode(&avif_data, &config)?;
    println!("AVIF decoded successfully");
    
    // Convert to RGB image
    let image = avif.convert()?;
    println!("Converted to RGB format");
    
    // Print image information
    match &image {
        Image::RGB8(img) => {
            println!("RGB8 image: {}x{}", img.width(), img.height());
            save_as_png(img, "decoded_output.png")?;
        }
        Image::RGBA8(img) => {
            println!("RGBA8 image: {}x{}", img.width(), img.height());
            save_as_png_rgba(img, "decoded_output.png")?;
        }
        other => {
            println!("Image format: {:?}", std::mem::discriminant(other));
        }
    }
    
    println!("AVIF decoding completed successfully!");
    Ok(())
}

fn save_as_png(img: &imgref::ImgVec<rgb::RGB<u8>>, path: &str) -> Result<()> {
    // Convert to raw bytes and create image buffer
    let width = img.width() as u32;
    let height = img.height() as u32;
    
    // Convert RGB data to flat bytes
    let mut rgb_bytes = Vec::with_capacity((width * height * 3) as usize);
    for pixel in img.buf() {
        rgb_bytes.push(pixel.r);
        rgb_bytes.push(pixel.g);
        rgb_bytes.push(pixel.b);
    }
    
    let image_buf = image::ImageBuffer::from_raw(width, height, rgb_bytes)
        .ok_or_else(|| anyhow!("Failed to create image buffer"))?;
    
    let dynamic_image = image::DynamicImage::ImageRgb8(image_buf);
    dynamic_image.save(path)?;
    println!("Saved RGB image as {}", path);
    Ok(())
}

fn save_as_png_rgba(img: &imgref::ImgVec<rgb::RGBA<u8>>, path: &str) -> Result<()> {
    // Convert to raw bytes and create image buffer
    let width = img.width() as u32;
    let height = img.height() as u32;
    
    // Convert RGBA data to flat bytes
    let mut rgba_bytes = Vec::with_capacity((width * height * 4) as usize);
    for pixel in img.buf() {
        rgba_bytes.push(pixel.r);
        rgba_bytes.push(pixel.g);
        rgba_bytes.push(pixel.b);
        rgba_bytes.push(pixel.a);
    }
    
    let image_buf = image::ImageBuffer::from_raw(width, height, rgba_bytes)
        .ok_or_else(|| anyhow!("Failed to create image buffer"))?;
    
    let dynamic_image = image::DynamicImage::ImageRgba8(image_buf);
    dynamic_image.save(path)?;
    println!("Saved RGBA image as {}", path);
    Ok(())
}
