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
    match parse_pair(s, ',') {
        Some((re, im)) => Some(Complex { re, im }),
        None => None,
    }
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
