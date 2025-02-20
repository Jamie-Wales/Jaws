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
    libstdc++-dev

# Set clang as the default compiler
ENV CC=clang
ENV CXX=clang++

WORKDIR /app
CMD ["/bin/sh"]
