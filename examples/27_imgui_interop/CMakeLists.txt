cmake_minimum_required(VERSION 3.22)
project(RustCppImGui)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Corrosion builds the Rust staticlib (rust_logic) inside CMake.
include(FetchContent)
FetchContent_Declare(
  Corrosion
  GIT_REPOSITORY https://github.com/corrosion-rs/corrosion.git
  GIT_TAG v0.6.1
)
FetchContent_MakeAvailable(Corrosion)

# Dear ImGui sources, pinned (not a floating branch).
FetchContent_Declare(
  imgui
  GIT_REPOSITORY https://github.com/ocornut/imgui.git
  GIT_TAG v1.92.9b
)
FetchContent_MakeAvailable(imgui)

find_package(glfw3 REQUIRED)
find_package(OpenGL REQUIRED)

corrosion_import_crate(MANIFEST_PATH rust_logic/Cargo.toml)

add_executable(my_interop_app
  src/main.cpp
  ${imgui_SOURCE_DIR}/imgui.cpp
  ${imgui_SOURCE_DIR}/imgui_draw.cpp
  ${imgui_SOURCE_DIR}/imgui_tables.cpp
  ${imgui_SOURCE_DIR}/imgui_widgets.cpp
  ${imgui_SOURCE_DIR}/backends/imgui_impl_glfw.cpp
  ${imgui_SOURCE_DIR}/backends/imgui_impl_opengl3.cpp
)

target_include_directories(my_interop_app PRIVATE
  ${imgui_SOURCE_DIR}
  ${imgui_SOURCE_DIR}/backends
)

target_link_libraries(my_interop_app PRIVATE
  rust_logic
  glfw
  OpenGL::GL
)
