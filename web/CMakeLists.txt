cmake_minimum_required(VERSION 3.14)
project(jaws_wasm)

set(CMAKE_TOOLCHAIN_FILE $ENV{EMSDK}/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Collect source files but exclude main.cpp
file(GLOB SOURCES
    "../src/compiler/*.cpp"
)
list(FILTER SOURCES EXCLUDE REGEX "main\\.cpp$")

# Add the wrapper file
list(APPEND SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/JawsWrapper.cpp")

add_executable(jaws_wasm ${SOURCES})

target_compile_definitions(jaws_wasm PRIVATE
    WASM_BUILD
)

# Update link flags to include embind
set_target_properties(jaws_wasm PROPERTIES
    LINK_FLAGS "-s WASM=1 \
                -s EXPORTED_RUNTIME_METHODS=['ccall','cwrap'] \
                -s EXPORTED_FUNCTIONS=['_malloc','_free'] \
                -s ALLOW_MEMORY_GROWTH=1 \
                -s NO_EXIT_RUNTIME=1 \
                -s MODULARIZE=1 \
                -s EXPORT_NAME='JawsInterpreter' \
                -s ENVIRONMENT='web' \
                -s STRICT=1 \
                -s DISABLE_EXCEPTION_CATCHING=0 \
                -s WASM_BIGINT=1 \
                -lembind \
                --bind \
                --no-entry"
    OUTPUT_NAME "jaws"
)

# Fix include paths to be relative to project root
target_include_directories(jaws_wasm PUBLIC
    "${CMAKE_CURRENT_SOURCE_DIR}/../include"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/compiler"
)

# Add compile options for better debugging
target_compile_options(jaws_wasm PRIVATE
    -fexceptions
    -g3
    -O0
)
