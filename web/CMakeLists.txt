# web/CMakeLists.txt
cmake_minimum_required(VERSION 3.14)
project(jaws_wasm)

set(CMAKE_TOOLCHAIN_FILE $ENV{EMSDK}/upstream/emscripten/cmake/Modules/Platform/Emscripten.cmake)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
file(GLOB SOURCES


    "../src/utils/*.cpp"
    "../src/interpreter/*.cpp"
    "../src/frontend/*.cpp"
    "../src/compiler/*.cpp"
    "../src/interpreter/builtins/*.cpp"
)
list(FILTER SOURCES EXCLUDE REGEX "main\\.cpp$")
list(APPEND SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/JawsWrapper.cpp")

add_executable(jaws_wasm ${SOURCES})

target_compile_definitions(jaws_wasm PRIVATE WASM_BUILD)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set_target_properties(jaws_wasm PROPERTIES
    LINK_FLAGS "-s WASM=1 \
                -s EXPORTED_RUNTIME_METHODS=['ccall','cwrap'] \
                -s EXPORTED_FUNCTIONS=['_malloc','_free'] \
                -s ALLOW_MEMORY_GROWTH=1 \
                -s NO_EXIT_RUNTIME=1 \
                -s MODULARIZE=1 \
                -s EXPORT_NAME='createJawsModule' \
                -s ENVIRONMENT='web' \
                -s DISABLE_EXCEPTION_CATCHING=0 \
                -s STACK_SIZE=8MB \
                -s WASM_BIGINT=1 \
                -sASSERTIONS=1 \
                -sASSERTIONS=2 \
                -lembind \
                --bind \
                --no-entry"
    OUTPUT_NAME "jaws"
)

target_include_directories(jaws_wasm PUBLIC
    "${CMAKE_CURRENT_SOURCE_DIR}/../include"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/frontend"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/compiler"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/utils"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/interpreter"

    "${CMAKE_CURRENT_SOURCE_DIR}/../include/interpreter/builtins"
)
# Copy to the client directory inside web
add_custom_command(TARGET jaws_wasm POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_SOURCE_DIR}/client/public/wasm"
    COMMAND ${CMAKE_COMMAND} -E copy
        "${CMAKE_CURRENT_BINARY_DIR}/jaws.js"
        "${CMAKE_CURRENT_SOURCE_DIR}/client/public/wasm/jaws.js"
    COMMAND ${CMAKE_COMMAND} -E copy
        "${CMAKE_CURRENT_BINARY_DIR}/jaws.wasm"
        "${CMAKE_CURRENT_SOURCE_DIR}/client/public/wasm/jaws.wasm"
    COMMENT "Copying WASM files to web/client/public/wasm directory"
)
