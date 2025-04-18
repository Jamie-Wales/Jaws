cmake_minimum_required(VERSION 3.14)
project(jaws_runtime C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(APPLE)
    set(CMAKE_OSX_ARCHITECTURES "arm64")
    set(CMAKE_OSX_DEPLOYMENT_TARGET "11.0" CACHE STRING "Minimum OS X deployment version")
else()
    set(CMAKE_SYSTEM_PROCESSOR "x86_64")
endif()

file(GLOB_RECURSE RUNTIME_SOURCES "${PROJECT_SOURCE_DIR}/src/*.c")
file(GLOB_RECURSE RUNTIME_HEADERS "${PROJECT_SOURCE_DIR}/include/*.h")

# Create the runtime static library
add_library(runtime STATIC
    ${RUNTIME_SOURCES}
    ${RUNTIME_HEADERS}
)

target_include_directories(runtime
    PUBLIC
        ${PROJECT_SOURCE_DIR}/include
    PRIVATE
        ${PROJECT_SOURCE_DIR}/src
)

set_target_properties(runtime PROPERTIES
    C_STANDARD 17
    C_STANDARD_REQUIRED ON
    POSITION_INDEPENDENT_CODE ON
)

# Platform-specific compiler flags
if(APPLE)
    target_compile_options(runtime PRIVATE
        -Wall
        -Wextra
        -Wpedantic
    )
    target_compile_options(runtime PRIVATE
        -Wall
        -Wextra
        -m64
        -fPIC
    )
endif()

# Link against libc explicitly
target_link_libraries(runtime PRIVATE c)

install(TARGETS runtime
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
)

install(DIRECTORY ${PROJECT_SOURCE_DIR}/include/
    DESTINATION include/jaws_runtime
    FILES_MATCHING PATTERN "*.h"
)
