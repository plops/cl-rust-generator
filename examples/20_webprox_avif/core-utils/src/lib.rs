use yuv::{
    rgb_to_yuv420,
    yuv420_to_rgb,
    YuvPlanarImageMut,
    YuvPlanarImage,
    YuvChromaSubsampling,
    YuvRange,
    YuvStandardMatrix,
    YuvConversionMode,
};

pub struct ImageRgba {
    pub width: u32,
    pub height: u32,
    pub pixels: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct YuvPlanarImageDto {
    pub width: u32,
    pub height: u32,
    pub y: Vec<u8>,
    pub u: Vec<u8>,
    pub v: Vec<u8>,
    pub y_stride: u32,
    pub u_stride: u32,
    pub v_stride: u32,
}

pub fn rgba_to_yuv420(rgba: &[u8], width: u32, height: u32) -> Result<YuvPlanarImageDto, Box<dyn std::error::Error>> {
    let mut planar_image = YuvPlanarImageMut::<u8>::alloc(width, height, YuvChromaSubsampling::Yuv420);
    
    yuv::rgba_to_yuv420(
        &mut planar_image,
        rgba,
        width * 4,
        YuvRange::Limited,
        YuvStandardMatrix::Bt601,
        YuvConversionMode::Balanced,
    ).map_err(|e| format!("RGBA to YUV conversion failed: {:?}", e))?;

    let fixed = planar_image.to_fixed();

    Ok(YuvPlanarImageDto {
        width,
        height,
        y: fixed.y_plane.to_vec(),
        u: fixed.u_plane.to_vec(),
        v: fixed.v_plane.to_vec(),
        y_stride: fixed.y_stride,
        u_stride: fixed.u_stride,
        v_stride: fixed.v_stride,
    })
}

pub fn yuv420_to_rgba(yuv_dto: &YuvPlanarImageDto, rgba: &mut [u8]) -> Result<(), Box<dyn std::error::Error>> {
    let fixed = YuvPlanarImage {
        y_plane: &yuv_dto.y,
        y_stride: yuv_dto.y_stride,
        u_plane: &yuv_dto.u,
        u_stride: yuv_dto.u_stride,
        v_plane: &yuv_dto.v,
        v_stride: yuv_dto.v_stride,
        width: yuv_dto.width,
        height: yuv_dto.height,
    };

    yuv::yuv420_to_rgba(
        &fixed,
        rgba,
        yuv_dto.width * 4,
        YuvRange::Limited,
        YuvStandardMatrix::Bt601,
    ).map_err(|e| format!("YUV to RGBA conversion failed: {:?}", e))?;

    Ok(())
}
