cmake_minimum_required(VERSION 3.14)
project(jaws)

if(NOT APPLE)
    message(FATAL_ERROR "This project is currently only supported on macOS")
endif()

# Basic setup
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_OSX_DEPLOYMENT_TARGET "11.0" CACHE STRING "Minimum OS X deployment version")
set(CMAKE_OSX_ARCHITECTURES "arm64")

# Enable testing
include(CTest)
enable_testing()

# Fetch dependencies
include(FetchContent)

# GoogleTest setup
FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/refs/tags/release-1.12.1.zip
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)


# Make dependencies available
FetchContent_MakeAvailable(googletest)

# Collect source files
file(GLOB_RECURSE COMPILER_SOURCES "${PROJECT_SOURCE_DIR}/src/*.cpp")
file(GLOB_RECURSE RUNTIME_SOURCES "${PROJECT_SOURCE_DIR}/runtime/src/*.c")
file(GLOB_RECURSE TEST_SOURCES "${PROJECT_SOURCE_DIR}/tests/*.cpp")

# Exclude main.cpp from compiler sources
list(FILTER COMPILER_SOURCES EXCLUDE REGEX "main\\.cpp$")

# Create the compiler library
add_library(compiler_lib STATIC ${COMPILER_SOURCES})
target_include_directories(compiler_lib PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/include/compiler
    ${PROJECT_SOURCE_DIR}/include/interpreter/builtins
    ${PROJECT_SOURCE_DIR}/include/interpreter
    ${PROJECT_SOURCE_DIR}/include/utils
    ${PROJECT_SOURCE_DIR}/include/frontend

)
set_target_properties(compiler_lib PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
)

# Create the runtime library
add_library(runtime STATIC ${RUNTIME_SOURCES})
target_include_directories(runtime PUBLIC
    ${PROJECT_SOURCE_DIR}/include/runtime
)
set_target_properties(runtime PROPERTIES
    C_STANDARD 17
    C_STANDARD_REQUIRED ON
)

# Create the jaws executable
add_executable(jaws src/main.cpp)
target_link_libraries(jaws
    PRIVATE compiler_lib
    runtime
)

target_include_directories(jaws PRIVATE
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/include/compiler
    ${PROJECT_SOURCE_DIR}/include/interpreter/builtins
    ${PROJECT_SOURCE_DIR}/include/interpreter
    ${PROJECT_SOURCE_DIR}/include/utils
    ${PROJECT_SOURCE_DIR}/include/frontend
)
set_target_properties(jaws PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
)

# Find assembler and linker
find_program(AS_PATH as)
find_program(LD_PATH ld)
if(NOT AS_PATH OR NOT LD_PATH)
    message(FATAL_ERROR "Could not find assembler (as) or linker (ld). Make sure Xcode Command Line Tools are installed.")
endif()

# Set compile definitions
target_compile_definitions(jaws PRIVATE
    AS_PATH="${AS_PATH}"
    LD_PATH="${LD_PATH}"
    RUNTIME_INCLUDE_PATH="${PROJECT_SOURCE_DIR}/include/runtime"
)

# Debug output
message(STATUS "Compiler sources: ${COMPILER_SOURCES}")
message(STATUS "Runtime sources: ${RUNTIME_SOURCES}")

# Create the test executable
add_executable(tests ${TEST_SOURCES})
target_include_directories(tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/include/compiler
    ${gtest_SOURCE_DIR}/include
)
set_target_properties(tests PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
)
target_link_libraries(tests
    PRIVATE compiler_lib
    runtime
    gtest_main
)
add_test(NAME AllTests COMMAND tests)
