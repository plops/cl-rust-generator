pub mod graphical_proxy {
    // The tonic/prost generated service traits return Result<_, tonic::Status>,
    // and tonic::Status is ~176 bytes.  Nothing we can change from here, so the
    // lint is silenced for the generated module only.
    #![allow(clippy::result_large_err)]

    tonic::include_proto!("graphical_proxy");
}
