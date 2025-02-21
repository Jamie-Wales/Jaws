#!/bin/bash

# Exit on any error
set -e

# Make sure we're in the right directory
cd "$(dirname "$0")"

echo "Building runtime..."
mkdir -p runtime/build
cd runtime/build
cmake ..
make
cd ../../

echo "Building generated assembly..."
# Use the correct path for output.o
gcc -o build/_prog build/build/output.o runtime/build/libruntime.a

echo "Build successful!"
echo "Run with: ./build/_prog"

# Optionally run the program
if [ "$1" = "--run" ]; then
    echo "Running program..."
    ./build/_prog
fi
