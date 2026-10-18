source6             .. initial program, based on previous onnx experiments
source7_mvp         .. fix resolution to 640x640
source8_transpiled  .. transpiler input to create 7 using cl-rust-generator
source9_gpu         .. unload some of the processing to the GPU (detecting text)
source10_instrumentation .. like 9 but with instrumentation to collect real world performance for channel characterization, further reduction in datarate and latency optimization
source11_compact    .. try to make protocol better and code more compact (but latency is actually worse, i tried to make authorization and tunnel more automatic (still needs automatic server start), also add android client (this needs automatic tunnel the most))


best implementation currently is source10
