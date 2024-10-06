cmake_minimum_required(VERSION 3.10)
project(jecl)

if(NOT APPLE)
    message(FATAL_ERROR "This project is currently only supported on macOS")
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_OSX_DEPLOYMENT_TARGET "11.0" CACHE STRING "Minimum OS X deployment version")

set(CMAKE_OSX_ARCHITECTURES "arm64")

file(GLOB_RECURSE COMPILER_SOURCES "${PROJECT_SOURCE_DIR}/src/compiler/*.cpp")
file(GLOB_RECURSE RUNTIME_SOURCES "${PROJECT_SOURCE_DIR}/runtime/src/*.c")

add_executable(jecl ${COMPILER_SOURCES})
target_include_directories(jecl PRIVATE
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/include/compiler
)

set_target_properties(jecl PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
)

add_library(runtime STATIC ${RUNTIME_SOURCES})
target_include_directories(runtime PUBLIC
    ${PROJECT_SOURCE_DIR}/include/runtime
)
set_target_properties(runtime PROPERTIES
    C_STANDARD 17
    C_STANDARD_REQUIRED ON
)

target_link_libraries(jecl PRIVATE runtime)

find_program(AS_PATH as)
find_program(LD_PATH ld)

if(NOT AS_PATH OR NOT LD_PATH)
    message(FATAL_ERROR "Could not find assembler (as) or linker (ld). Make sure Xcode Command Line Tools are installed.")
endif()

target_compile_definitions(jecl PRIVATE
    AS_PATH="${AS_PATH}"
    LD_PATH="${LD_PATH}"
    RUNTIME_INCLUDE_PATH="${PROJECT_SOURCE_DIR}/include/runtime"
)

get_property(dirs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY INCLUDE_DIRECTORIES)
foreach(dir ${dirs})
    message(STATUS "Include dir='${dir}'")
endforeach()

message(STATUS "Compiler sources: ${COMPILER_SOURCES}")
message(STATUS "Runtime sources: ${RUNTIME_SOURCES}")
