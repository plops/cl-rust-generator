extern crate iron;
use iron::prelude::*;
use iron::status;
fn get_form(_request: &mut Request) -> IronResult<Response> {
    let mut response = Response::new();
    response.set_mut(status::Ok);
    response.set_mut(
        r#"<title>GCD Calculator</title>
<form action='/gcd' method='post'>
<input type='text' name='n'/>
<input type='text' name='m'/>
<button type='submit'>Compute GCD</button>
</form>"#,
    );
    return Ok(response);
}
fn main() {
    println!("Serving on http://localhost:3000...");
    Iron::new(get_form).http("localhost:3000").unwrap();
}
