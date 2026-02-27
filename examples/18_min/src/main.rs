#![no_std]
#![no_main]

use core::arch::asm;
use core::panic::PanicInfo;

// Minimal syscall wrapper for Linux x86_64
unsafe fn syscall(num: u64, a1: u64, a2: u64, a3: u64) -> i64 {
    let ret: i64;
    asm!(
        "syscall",
        in("rax") num,
        in("rdi") a1,
        in("rsi") a2,
        in("rdx") a3,
        out("rcx") _,
        out("r11") _,
        lateout("rax") ret,
    );
    ret
}

#[unsafe(no_mangle)]
pub extern "C" fn _start() -> ! {
    unsafe {
        // 1. Create Socket (AF_INET = 2, SOCK_STREAM = 1, IPPROTO_IP = 0)
        let fd = syscall(41, 2, 1, 0);
        
        if fd >= 0 {
            // 2. Connect to 93.184.216.34 (example.com) on port 80
            // The struct sockaddr_in is represented here as a byte array
            // 0x02, 0x00 (AF_INET), 0x00, 0x50 (Port 80), 0x5D, 0xB8, 0xD8, 0x22 (IP)
            let sockaddr: [u8; 16] = [
                0x02, 0x00, 0x00, 0x50, 0x5D, 0xB8, 0xD8, 0x22, 
                0, 0, 0, 0, 0, 0, 0, 0
            ];
            
            let conn = syscall(42, fd as u64, sockaddr.as_ptr() as u64, 16);
            
            if conn == 0 {
                // 3. Write HTTP GET request
                let msg = b"GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n";
                syscall(1, fd as u64, msg.as_ptr() as u64, msg.len() as u64);
                
                // 4. Read response (minimal buffer on stack)
                let mut buf = [0u8; 256];
                syscall(0, fd as u64, buf.as_mut_ptr() as u64, buf.len() as u64);
            }
        }

        // 5. Exit (Syscall 60)
        syscall(60, 0, 0, 0);
    }
    loop {}
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}