# Force the amd64 architecture
FROM --platform=linux/amd64 alpine:latest

# Install build requirements
RUN apk update && \
    apk add --no-cache \
    nasm \
    clang \
    clang-dev \
    cmake \
    make \
    libc-dev \
    libstdc++ \
    libstdc++-dev \
    bash \
    gcc \
    g++

# Set clang as the default compiler
ENV CC=clang
ENV CXX=clang++

WORKDIR /app

# Add /app to PATH
ENV PATH="/app:${PATH}"

# Use bash as the shell
SHELL ["/bin/bash", "-c"]

# Make sure bash is available for interactive use
RUN ln -sf /bin/bash /bin/sh
