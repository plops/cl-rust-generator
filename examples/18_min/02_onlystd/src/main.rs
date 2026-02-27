use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};
use std::alloc::{alloc, dealloc, Layout};
use std::ffi::CString;
use std::os::raw::{c_char, c_int};

// FFI: Import the C 'printf' function from libc
unsafe extern "C" {
    fn printf(format: *const c_char, ...) -> c_int;
}

fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:8080")?;
    let msg = "--- Gentoo Rust Toy: Listening on 8080 ---\n";
    
    // FFI + ALLOC: Print startup message using C's printf
    unsafe {
        let fmt = CString::new("%s").unwrap();
        let c_msg = CString::new(msg).unwrap();
        printf(fmt.as_ptr(), c_msg.as_ptr());
    }

    for stream in listener.incoming() {
        match stream {
            Ok(mut s) => { handle_client(&mut s); }
            Err(e) => { eprintln!("Connection failed: {}", e); }
        }
    }
    Ok(())
}

fn handle_client(stream: &mut TcpStream) {
    let mut buffer = [0; 512];
    if let Ok(size) = stream.read(&mut buffer) {
        if size == 0 { return; }

        // ALLOC: Manually allocate heap memory for the incoming data
        // This bypasses Vec/Box to show raw std::alloc usage
        let layout = Layout::array::<u8>(size + 1).unwrap();
        unsafe {
            let ptr = alloc(layout);
            if !ptr.is_null() {
                // Copy data and add a null-terminator for C compatibility
                std::ptr::copy_nonoverlapping(buffer.as_ptr(), ptr, size);
                *ptr.add(size) = 0; 

                // FFI: Use C's printf to log the "foreign" string
                let fmt = CString::new("Client said: %s\n").unwrap();
                printf(fmt.as_ptr(), ptr as *const c_char);

                // NET: Echo back to the client using Rust's safe I/O
                let _ = stream.write_all(std::slice::from_raw_parts(ptr, size));
                
                // ALLOC: Manual deallocation (The "unsafe" way)
                dealloc(ptr, layout);
            }
        }
    }
}
