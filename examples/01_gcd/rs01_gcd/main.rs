use std::io::Write;
use std::str::FromStr;
fn gcd(mut n: u64, mut m: u64) -> u64 {
    assert!((((0) != (n)) && ((0) != (m))));
    while ((0) != (m)) {
        if (m < n) {
            let tt = m;
            m = n;
            n = tt;
        };
        m = m % n;
    }
    return n;
}
#[test]
fn test_gcd() {
    assert_eq!(gcd(14, 15), 1);
    assert_eq!(
        gcd(
            ((2) * (3) * (5) * (11) * (17)),
            ((3) * (7) * (11) * (13) * (19))
        ),
        ((3) * (11))
    );
}
fn main() {
    let mut numbers = Vec::new();
    for arg in std::env::args().skip(1) {
        numbers.push(u64::from_str(&arg).expect("error parsing argument"));
    }
    if ((0) == (numbers.len())) {
        writeln!(std::io::stderr(), "Usage: gcd NUMBER ...").unwrap();
        std::process::exit(1);
    };
    let mut d = numbers[0];
    for m in &(numbers[1..]) {
        d = gcd(d, *m);
    }
    println!("The greatest common divisor of {:?} is {}", numbers, d);
}
