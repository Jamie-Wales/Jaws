cmake_minimum_required(VERSION 3.14)
project(jaws)

if (NOT APPLE)
    message(FATAL_ERROR "This project is currently only supported on macOS")
endif ()

# Basic setup
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_OSX_DEPLOYMENT_TARGET "11.0" CACHE STRING "Minimum OS X deployment version")
set(CMAKE_OSX_ARCHITECTURES "arm64")


include(CTest)
enable_testing()

cmake_policy(SET CMP0135 NEW)

include(FetchContent)
# Fetch GoogleTest
FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/refs/tags/release-1.12.1.zip
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

FetchContent_Declare(
    tbb
    GIT_REPOSITORY https://github.com/oneapi-src/oneTBB.git
    GIT_TAG master # Use main branch for latest development version
)
# Configure TBB build options
set(TBB_TEST OFF CACHE BOOL "Enable testing")
set(TBB_EXAMPLES OFF CACHE BOOL "Enable examples")

# Make all the content available at once
FetchContent_MakeAvailable(googletest tbb)

# Find assembler and linker early
find_program(AS_PATH as)
find_program(LD_PATH ld)
if (NOT AS_PATH OR NOT LD_PATH)
    message(FATAL_ERROR "Could not find assembler (as) or linker (ld). Make sure Xcode Command Line Tools are installed.")
endif ()


# Collect source files
file(GLOB_RECURSE COMPILER_SOURCES
        "${PROJECT_SOURCE_DIR}/src/compiler/*.cpp"
        "${PROJECT_SOURCE_DIR}/src/frontend/*.cpp"
        "${PROJECT_SOURCE_DIR}/src/frontend/MacroEnvironment.cpp"
        "${PROJECT_SOURCE_DIR}/src/interpreter/*.cpp"
        "${PROJECT_SOURCE_DIR}/src/utils/*.cpp"
)
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
        ${PROJECT_SOURCE_DIR}/src/frontend/Environment.cpp # NOTE: Ensure correct path if it differs
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
        # Add JawsThread.cpp if it's not caught by GLOB_RECURSE or listed above
        ${PROJECT_SOURCE_DIR}/src/interpreter/builtins/JawsThread.cpp
)

target_include_directories(app_lib PUBLIC
        ${PROJECT_SOURCE_DIR}/include
        ${PROJECT_SOURCE_DIR}/include/frontend
        # These specific headers might be redundant if include/frontend is already listed
        # ${PROJECT_SOURCE_DIR}/include/frontend/Expression.h
        # ${PROJECT_SOURCE_DIR}/include/frontend/Token.h
        ${PROJECT_SOURCE_DIR}/include/compiler
        ${PROJECT_SOURCE_DIR}/include/interpreter/builtins
        ${PROJECT_SOURCE_DIR}/include/interpreter
        ${PROJECT_SOURCE_DIR}/include/utils
)

# Add compile definitions to app_lib
# Ensure these variables are defined earlier or remove if not needed
# target_compile_definitions(app_lib PUBLIC
#         AS_PATH="${AS_PATH}"
#         LD_PATH="${LD_PATH}"
#         RUNTIME_INCLUDE_PATH="${RUNTIME_INCLUDE_DIR}" # Define RUNTIME_INCLUDE_DIR first
#         RUNTIME_LIB_PATH="${RUNTIME_LIB_PATH}"       # Define RUNTIME_LIB_PATH first
# )

# Link TBB to app_lib
target_link_libraries(app_lib PUBLIC tbb)

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

target_link_libraries(jaws PRIVATE app_lib)

# Link filesystem library if needed (Keep this check)
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_link_libraries(app_lib PRIVATE stdc++fs)
    target_link_libraries(jaws PRIVATE stdc++fs)
endif()

# Tests setup
add_executable(tests ${TEST_SOURCES})
target_include_directories(tests PRIVATE
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/include/compiler
    ${PROJECT_SOURCE_DIR}/include/interpreter
    ${PROJECT_SOURCE_DIR}/include/interpreter/builtins
    ${PROJECT_SOURCE_DIR}/include/utils
    ${PROJECT_SOURCE_DIR}/include/frontend
    ${googletest_SOURCE_DIR}/include # Corrected variable name maybe? Check FetchContent details
)

set_target_properties(tests PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED ON
)

target_link_libraries(tests PRIVATE
    app_lib
    gtest_main
)


# <<< --- ADDITIONS FOR MEMORY DEBUGGING (ASan) --- >>>

# Define the flags needed for ASan and debugging
# Using semicolon-separated list for target_compile_options
set(MEMORY_DEBUG_COMPILE_FLAGS "-g;-O0;-fsanitize=address;-fno-omit-frame-pointer")

message(STATUS "Adding ASan/Debug compile flags: ${MEMORY_DEBUG_COMPILE_FLAGS}")

# Apply the compile flags to your library and executables
# The compiler driver (clang++ on macOS) usually handles passing
# the necessary sanitizer options to the linker automatically.
target_compile_options(app_lib PRIVATE ${MEMORY_DEBUG_COMPILE_FLAGS})
target_compile_options(jaws PRIVATE ${MEMORY_DEBUG_COMPILE_FLAGS})
target_compile_options(tests PRIVATE ${MEMORY_DEBUG_COMPILE_FLAGS})

target_link_options(jaws PRIVATE "-fsanitize=address")
target_link_options(tests PRIVATE "-fsanitize=address")

# <<< --- END OF ADDITIONS --- >>>


include(GoogleTest)
gtest_discover_tests(tests)
