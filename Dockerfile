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
    g++ \
    gdb \
    strace \
    binutils \
    ltrace

# Set clang as the default compiler
ENV CC=clang
ENV CXX=clang++

WORKDIR /app

ENV PATH="/app:${PATH}"

SHELL ["/bin/bash", "-c"]

RUN ln -sf /bin/bash /bin/sh
