cmake_minimum_required(VERSION 3.14)
project(jaws_runtime)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

file(GLOB_RECURSE RUNTIME_SOURCES "${PROJECT_SOURCE_DIR}/src/*.c")
file(GLOB_RECURSE INCLUDE_DIRECTORIES "${PROJECT_SOURCE_DIR}/include/*.h")


add_executable(jaws_runtime ${RUNTIME_SOURCES})

target_include_directories(jaws_runtime PRIVATE INCLUDE_DIRECTORIES)

set_target_properties(jaws_runtime PROPERTIES
    C_STANDARD 17
    C_STANDARD_REQUIRED ON
)
