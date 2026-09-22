#[repr(C)]
pub struct AppState {
    pub click_count: i32,
    pub slider_value: f32,
    pub checkbox_status: bool,
}
#[unsafe(no_mangle)]
pub extern "C" fn init_app_state() -> AppState {
    AppState {
        click_count: 0,
        slider_value: 0.50,
        checkbox_status: false,
    }
}
/// Advances the slider while the box is checked, wraps past 1.0.
///
/// # Safety
///
/// `state` must point to a live mutable `AppState`. Null is a no-op,
/// any other dangling pointer is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn process_logic(state: *mut AppState) {
    if state.is_null() {
        return;
    }
    unsafe {
        let s = &mut *state;
        if s.checkbox_status {
            s.slider_value += 1.00e-3;
            if 1.0 < s.slider_value {
                s.slider_value = 0.0;
            }
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn t_init_defaults() {
        let s = init_app_state();
        assert!(s.click_count == 0);
        assert!(s.slider_value == 0.50);
        assert!(!s.checkbox_status);
    }
    #[test]
    fn t_null_noop() {
        unsafe { process_logic(std::ptr::null_mut()) }
    }
    #[test]
    fn t_inactive_keeps_slider() {
        let mut s = init_app_state();
        unsafe { process_logic(&mut s) }
        assert!(s.slider_value == 0.50);
    }
    #[test]
    fn t_active_steps_slider() {
        let mut s = init_app_state();
        s.checkbox_status = true;
        unsafe { process_logic(&mut s) }
        assert!(0.50 < s.slider_value);
        assert!(s.slider_value < 0.60);
    }
    #[test]
    fn t_wrap_at_one() {
        let mut s = init_app_state();
        s.checkbox_status = true;
        s.slider_value = 1.0;
        unsafe { process_logic(&mut s) }
        assert!(s.slider_value == 0.0);
    }
}
