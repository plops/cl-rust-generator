Cargo.toml for profiling:
[profile.release]
opt-level = 3
debug = 1             # Adds function names/line tables for profilers
strip = false         # Do not strip symbols!


cargo install samply

    (If on Linux, ensure perf events are allowed: echo 1 | sudo tee /proc/sys/kernel/perf_event_paranoid)

    Run your app under samply:
    Use --headless so you don't profile Macroquad waiting for VSync:


    cargo build --release
    samply record ./target/release/x11_rb_mq_viewer --headless --fps 30


profile with perf:

perf record -F 99 -g -- ./target/release/x11_rb_mq_viewer --headless --fps 30
# Press Ctrl+C after a few seconds
perf report

my kernel misses some options
 *   CONFIG_SCHED_OMIT_FRAME_POINTER:    should not be set. But it is.
 *   CONFIG_DEBUG_INFO:  is not set when it should be.
 *   CONFIG_FRAME_POINTER:       is not set when it should be.

but it still seems to work:

Samples: 2K of event 'cpu/cycles/P', Event count (approx.): 60403801270
  Children      Self  Command          Shared Object         Symbol
+   32.02%    29.62%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] MlasConvNchwcFloatKernelFma3                     ◆
+   19.93%    19.51%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] MlasConvPointwiseFloatKernelFma3                 ▒
+    7.93%     0.15%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] do_syscall_64                                    ▒
+    7.78%     0.09%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] entry_SYSCALL_64_after_hwframe                   ▒
+    6.85%     0.05%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] __x64_sys_futex                                  ▒
+    6.68%     6.02%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] MlasComputeLogisticF32KernelFma3                 ▒
+    6.57%     0.00%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] do_futex                                         ▒
+    6.05%     0.00%  x11_rb_mq_viewe  libc.so.6             [.] 0x00007ff2e2f0f6a2                               ▒
+    4.95%     0.12%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] futex_wait                                       ▒
+    4.80%     0.03%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] __futex_wait                                     ▒
+    4.30%     0.11%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] schedule                                         ▒
+    4.24%     0.07%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] futex_do_wait                                    ▒
+    4.16%     3.69%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] void MlasEltwiseMul<float>(float const*, float co▒
+    4.01%     1.25%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] MlasConvPointwiseFloatKernelAvx                  ▒
+    3.72%     0.10%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] __schedule                                       ▒
+    3.08%     2.77%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] source1::infer::infer_image                      ▒
+    2.80%     2.67%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] MlasConvNchwFloatKernelFma3                      ▒
+    2.77%     1.89%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] onnxruntime::concurrency::SpinPause()            ▒
+    2.32%     1.51%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] MlasConvNchwcFloatSingleFma3Filter4              ▒
+    2.10%     1.41%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] entry_SYSRETQ_unsafe_stack                       ▒
     1.85%     1.81%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] MlasReorderInputNchw(float const*, float*, unsign▒
+    1.66%     0.00%  x11_rb_mq_viewe  libc.so.6             [.] 0x00007ff2e2fd69aa                               ▒
+    1.66%     0.12%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] futex_wake                                       ▒
+    1.56%     0.00%  x11_rb_mq_viewe  libc.so.6             [.] pthread_cond_signal                              ▒
+    1.25%     0.00%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] __pick_next_task                                 ▒
+    1.19%     1.19%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] onnxruntime::DoTransposeEltWise(long, gsl::span<l▒
+    1.18%     1.18%  x11_rb_mq_viewe  x11_rb_mq_viewer      [.] x11_rb_mq_viewer::process_frame                  ▒
+    1.17%     0.00%  x11_rb_mq_viewe  libc.so.6             [.] 0x00007ff2e2f04617                               ▒
+    1.17%     0.00%  x11_rb_mq_viewe  [kernel.kallsyms]     [k] wake_up_q                  
