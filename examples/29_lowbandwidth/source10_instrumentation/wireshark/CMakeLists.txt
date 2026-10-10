# Baut packet-lbw.c als Out-of-Tree-Plugin gegen das installierte
# libwireshark (libwireshark-dev). Das Plugin lädt nur in derselben
# Wireshark-Version (ABI-Prüfung) — deshalb fragt das Build die Version ab.
cmake_minimum_required(VERSION 3.16)
project(lbw-wireshark C)

find_package(PkgConfig REQUIRED)
pkg_check_modules(WS REQUIRED wireshark)
execute_process(
    COMMAND ${PKG_CONFIG_EXECUTABLE} --modversion wireshark
    OUTPUT_VARIABLE WS_VER OUTPUT_STRIP_TRAILING_WHITESPACE)
string(REGEX MATCH "^([0-9]+)\\.([0-9]+)" _ "${WS_VER}")
set(WS_MAJOR ${CMAKE_MATCH_1})
set(WS_MINOR ${CMAKE_MATCH_2})
message(STATUS "Wireshark ${WS_VER} (Plugin für ${WS_MAJOR}.${WS_MINOR})")

add_library(lbwpdu MODULE packet-lbw.c)
target_include_directories(lbwpdu PRIVATE ${WS_INCLUDE_DIRS})
target_compile_options(lbwpdu PRIVATE ${WS_CFLAGS_OTHER} -Wall -Wextra)
target_compile_definitions(lbwpdu PRIVATE
    LBW_PLUGIN_VERSION="${WS_VER}"
    LBW_WANT_MAJOR=${WS_MAJOR}
    LBW_WANT_MINOR=${WS_MINOR})
target_link_libraries(lbwpdu ${WS_LIBRARIES})
set_target_properties(lbwpdu PROPERTIES PREFIX "" OUTPUT_NAME "lbw" SUFFIX ".so")
