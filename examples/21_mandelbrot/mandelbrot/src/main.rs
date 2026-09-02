use num::Complex;
fn escape_time(c: Complex<f64>, limit: usize) -> Option<usize> {
    {
        let mut z = Complex { re: 0.0, im: 0.0 };
        for i in 0..limit {
            if (4.0) < (z.norm_sqr()) {
                return Some(i);
            } else {
                z = ((z) * (z)) + c;
            }
        }
    }
    None
}
use std::str::FromStr;
/// parse string s of the form <left><sep><right> like 400x600 or 1,5
fn parse_pair<T: FromStr>(s: &str, separator: char) -> Option<(T, T)> {
    match s.find(separator) {
        None => None,
        Some(index) => match (T::from_str((&s[..index])), T::from_str((&s[(index + 1..)]))) {
            (Ok(l), Ok(r)) => Some((l, r)),
            _ => None,
        },
    }
}
#[test]
fn test_parse_pair() {
    assert_eq!((parse_pair::<i32>)("", ','), None);
    assert_eq!((parse_pair::<i32>)("10,", ','), None);
    assert_eq!((parse_pair::<i32>)(",10", ','), None);
    assert_eq!((parse_pair::<i32>)("10,20", ','), Some((10, 20)));
    assert_eq!((parse_pair::<i32>)("10,20xy", ','), None);
    assert_eq!((parse_pair::<f64>)("0.5x", 'x'), None);
    assert_eq!((parse_pair::<f64>)("0.5x1.5", 'x'), Some((0.50, 1.50)))
}
fn parse_complex(s: &str) -> Option<Complex<f64>> {
    parse_pair(s, ',').map(|(re, im)| Complex { re, im })
}
#[test]
fn test_parse_complex() {
    assert_eq!(
        parse_complex("1.25,-.0625"),
        Some(Complex {
            re: 1.250,
            im: (-6.250e-2)
        })
    );
    assert_eq!(parse_complex(",-.06"), None)
}
/// Given row and column of a pixel in image grid return corresponding point on the complex plane
fn pixel_to_point(
    bounds: (usize, usize),
    pixel: (usize, usize),
    upper_left: Complex<f64>,
    lower_right: Complex<f64>,
) -> Complex<f64> {
    {
        let (width, height) = (
            (lower_right.re - upper_left.re),
            (upper_left.im - lower_right.im),
        );
        Complex {
            re: (upper_left.re + (((pixel.0 as f64) * (width)) / (bounds.0 as f64))),
            im: (upper_left.im - (((pixel.1 as f64) * (height)) / (bounds.1 as f64))),
        }
    }
}
#[test]
fn test_pixel_to_point() {
    assert_eq!(
        pixel_to_point(
            (100, 200),
            (25, 175),
            Complex {
                re: (-1.0),
                im: 1.0
            },
            Complex {
                re: 1.0,
                im: (-1.0)
            }
        ),
        Complex {
            re: (-0.50),
            im: (-0.750)
        }
    )
}
fn render(
    pixels: &mut [u8],
    bounds: (usize, usize),
    upper_left: Complex<f64>,
    lower_right: Complex<f64>,
) {
    assert!(((pixels.len()) == ((bounds.0) * (bounds.1))));
    for row in 0..bounds.1 {
        for column in 0..bounds.0 {
            {
                let point = pixel_to_point(bounds, (column, row), upper_left, lower_right);
                pixels[(((row) * (bounds.0)) + column)] = match escape_time(point, 65535) {
                    None => 0,
                    Some(count) => (((count as f32) / (3.894517e-5)).powf(0.30) as u8),
                };
            }
        }
    }
}
use image::codecs::webp::WebPEncoder;
use image::{ExtendedColorType, ImageEncoder, ImageError};
use std::fs::File;
fn write_image(filename: &str, pixels: &[u8], bounds: (usize, usize)) -> Result<(), ImageError> {
    {
        let output = File::create(filename)?;
        let encoder = WebPEncoder::new_lossless(output);
        encoder.write_image(
            pixels,
            (bounds.0 as u32),
            (bounds.1 as u32),
            ExtendedColorType::L8,
        )?;
        Ok(())
    }
}
use std::env;
fn main() {
    {
        let args: Vec<String> = env::args().collect();
        if (5) != (args.len()) {
            {
                let program = &args[0];
                eprintln!("Usage:   {program} FILE        PIXELS   LEFT,TOP RIGHT,BOTTOM");
                eprintln!("Example: {program} mandel.webp 1000x750 -1.2,.35 -1,.2");
                std::process::exit(1)
            }
        }
        {
            let bounds: (usize, usize) =
                parse_pair(&args[2], 'x').expect("error parsing image dimensions");
            let upper_left =
                parse_complex(&args[3]).expect("error parsing upper left corner point");
            let lower_right =
                parse_complex(&args[4]).expect("error parsing lower right corner point");
            {
                let mut pixels = vec![0; ((bounds.0) * (bounds.1))];
                {
                    let threads = std::thread::available_parallelism()
                        .expect("error querying CPU count")
                        .get();
                    let rows_per_band = bounds.1.div_ceil(threads);
                    let bands = pixels.chunks_mut(((rows_per_band) * (bounds.0)));
                    eprintln!("bounds={:?} upper_left={:?} lower_right={:?} threads={:?} rows_per_band={:?}", bounds, upper_left, lower_right, threads, rows_per_band);
                    std::thread::scope(|spawner| {
                        for (i, band) in bands.enumerate() {
                            {
                                let top = ((rows_per_band) * (i));
                                let height = ((band.len()) / (bounds.0));
                                let band_bounds = (bounds.0, height);
                                let band_upper_left =
                                    pixel_to_point(bounds, (0, top), upper_left, lower_right);
                                let band_lower_right = pixel_to_point(
                                    bounds,
                                    (bounds.0, (top + height)),
                                    upper_left,
                                    lower_right,
                                );
                                eprintln!("i={:?} top={:?} height={:?} band_bounds={:?} band_upper_left={:?} band_lower_right={:?}", i, top, height, band_bounds, band_upper_left, band_lower_right);
                                spawner.spawn(move || {
                                    render(band, band_bounds, band_upper_left, band_lower_right);
                                });
                            }
                        }
                    })
                }
                write_image(&args[1], &pixels, bounds).expect("error writing PNG file")
            }
        }
    }
}
