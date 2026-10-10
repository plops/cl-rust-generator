source6             .. initial program, based on previous onnx experiments
source7_mvp         .. fix resolution to 640x640
source8_transpiled  .. transpiler input to create 7 using cl-rust-generator
source9_gpu         .. unload some of the processing to the GPU (detecting text)
source10_log        .. like 9 but with instrumentation to collect real world performance for channel characterization, further reduction in datarate and latency optimization
