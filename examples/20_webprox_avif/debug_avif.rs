use aom_decode::{avif::{Avif, Image}, Config};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <avif_file>", args[0]);
        std::process::exit(1);
    }
    
    let file_data = std::fs::read(&args[1])?;
    println!("File size: {} bytes", file_data.len());
    
    // Show first few bytes
    println!("Header: {:02X?}", &file_data[..16]);
    
    // Configure decoder
    let config = Config {
        threads: std::thread::available_parallelism()?.get(),
    };
    
    // Decode AVIF
    println!("Decoding AVIF...");
    let mut avif = Avif::decode(&file_data, &config)?;
    println!("Decoded successfully!");
    
    // Convert to RGB
    let image = avif.convert()?;
    println!("Converted to RGB successfully!");
    
    match image {
        Image::RGB8(img) => {
            println!("RGB8 image: {}x{}", img.width(), img.height());
        }
        Image::RGBA8(img) => {
            println!("RGBA8 image: {}x{}", img.width(), img.height());
        }
        _ => {
            println!("Other image format");
        }
    }
    
    Ok(())
}
