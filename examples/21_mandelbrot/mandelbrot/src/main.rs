use num::Complex;
fn escape_time(c: Complex<f64>, limit: usize) -> Option<usize> {
    let mut z = Complex { re: 0.0, im: 0.0 };
    for i in 0..limit {
        if (4.0) < (z.norm_sqr()) {
            return Some(i);
        } else {
            z = ((z) * (z)) + c;
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
