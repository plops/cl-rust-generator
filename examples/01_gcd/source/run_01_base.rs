use std::io::Write;
use std::str::FromStr;
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
    println!("The greatest common divisor of {:?} is {}");
}
