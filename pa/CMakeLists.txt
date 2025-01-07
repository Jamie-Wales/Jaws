cmake_minimum_required(VERSION 3.15)
project(portaudio_wrapper)

# Find PortAudio package
find_library(PORTAUDIO portaudio PATHS /opt/homebrew/opt/portaudio/lib REQUIRED)
include_directories(/opt/homebrew/opt/portaudio/include)

# Create shared library
add_library(portaudio_wrapper SHARED portaudio_wrapper.c)
target_link_libraries(portaudio_wrapper ${PORTAUDIO})

# Set macOS specific properties
set_target_properties(portaudio_wrapper PROPERTIES
    MACOSX_RPATH ON
    BUILD_WITH_INSTALL_RPATH ON
    INSTALL_RPATH "@loader_path;/opt/homebrew/opt/portaudio/lib"
)
