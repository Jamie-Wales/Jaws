cmake_minimum_required(VERSION 3.14)
project(jaws)

if(NOT APPLE)
    message(FATAL_ERROR "This project is currently only supported on macOS")
endif()

# Basic setup
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_OSX_DEPLOYMENT_TARGET "11.0" CACHE STRING "Minimum OS X deployment version")
set(CMAKE_OSX_ARCHITECTURES "arm64")

include(CTest)
enable_testing()

include(FetchContent)
FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/refs/tags/release-1.12.1.zip
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

# Collect source files
file(GLOB_RECURSE COMPILER_SOURCES
    "${PROJECT_SOURCE_DIR}/src/compiler/*.cpp"
    "${PROJECT_SOURCE_DIR}/src/frontend/*.cpp"
    "${PROJECT_SOURCE_DIR}/src/interpreter/*.cpp"
    "${PROJECT_SOURCE_DIR}/src/utils/*.cpp"
)
file(GLOB_RECURSE RUNTIME_SOURCES "${PROJECT_SOURCE_DIR}/runtime/src/*.c")
file(GLOB_RECURSE TEST_SOURCES "${PROJECT_SOURCE_DIR}/tests/*.cpp")
file(GLOB MAIN_SOURCE "${PROJECT_SOURCE_DIR}/src/main.cpp")

# Create a library for main application
add_library(app_lib STATIC
    ${COMPILER_SOURCES}
    ${PROJECT_SOURCE_DIR}/src/interpreter/Number.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/Value.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/ValueTraits.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/Procedure.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/interpret.cpp
    ${PROJECT_SOURCE_DIR}/src/frontend/scan.cpp
    ${PROJECT_SOURCE_DIR}/src/frontend/parse.cpp
    ${PROJECT_SOURCE_DIR}/src/frontend/Expression.cpp
    ${PROJECT_SOURCE_DIR}/src/frontend/Environment.cpp
    ${PROJECT_SOURCE_DIR}/src/utils/run.cpp
    ${PROJECT_SOURCE_DIR}/src/utils/ExpressionUtils.cpp
    ${PROJECT_SOURCE_DIR}/src/utils/DebugUtils.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/builtins/JawsEq.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/builtins/JawsHof.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/builtins/JawsIO.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/builtins/JawsList.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/builtins/JawsMacro.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/builtins/JawsMath.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/builtins/JawsVector.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/builtins/jaws_ffi.cpp
    ${PROJECT_SOURCE_DIR}/src/interpreter/builtins/jaws_values.cpp
)

target_include_directories(app_lib PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/include/frontend
    ${PROJECT_SOURCE_DIR}/include/frontend/Expression.h
    ${PROJECT_SOURCE_DIR}/include/frontend/Token.h
    ${PROJECT_SOURCE_DIR}/include/compiler
    ${PROJECT_SOURCE_DIR}/include/interpreter/builtins
    ${PROJECT_SOURCE_DIR}/include/interpreter
    ${PROJECT_SOURCE_DIR}/include/utils
)

set_target_properties(app_lib PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
)

# Create the executable
add_executable(jaws ${MAIN_SOURCE})
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

# Create the runtime library
add_library(runtime STATIC ${RUNTIME_SOURCES})
target_include_directories(runtime PUBLIC
    ${PROJECT_SOURCE_DIR}/include/runtime
)

set_target_properties(runtime PROPERTIES
    C_STANDARD 17
    C_STANDARD_REQUIRED ON
)

target_link_libraries(jaws PRIVATE app_lib runtime)

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

# Create the test executable
add_executable(tests ${TEST_SOURCES})
target_include_directories(tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/include/compiler
    ${PROJECT_SOURCE_DIR}/include/interpreter
    ${PROJECT_SOURCE_DIR}/include/interpreter/builtins
    ${PROJECT_SOURCE_DIR}/include/utils
    ${PROJECT_SOURCE_DIR}/include/frontend
    ${gtest_SOURCE_DIR}/include
)

set_target_properties(tests PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
)

target_link_libraries(tests PRIVATE
    app_lib
    runtime
    gtest_main
)

add_test(NAME AllTests COMMAND tests)
