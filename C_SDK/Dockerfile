# =============================================================================
# DCF - Distributed Communication Framework
# Multi-stage Dockerfile for building and packaging
# =============================================================================
#
# Build arguments:
#   BUILD_TYPE    - Release, Debug, RelWithDebInfo, MinSizeRel (default: Release)
#   ENABLE_TESTS  - ON/OFF (default: ON)
#   ENABLE_LTO    - ON/OFF (default: ON)
#   SANITIZERS    - ON/OFF (default: OFF)
#   COVERAGE      - ON/OFF (default: OFF)
#
# Usage:
#   # Build release image
#   docker build -t dcf:latest .
#
#   # Build debug image
#   docker build --build-arg BUILD_TYPE=Debug -t dcf:debug .
#
#   # Build with sanitizers
#   docker build --build-arg SANITIZERS=ON --build-arg BUILD_TYPE=Debug -t dcf:asan .
#
#   # Run tests
#   docker run --rm dcf:latest /app/bin/dcf_tests
#
#   # Run example
#   docker run --rm dcf:latest /app/bin/example_basic
#
#   # Interactive development
#   docker run -it --rm -v $(pwd):/src dcf:dev bash
#
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Builder base
# -----------------------------------------------------------------------------
FROM ubuntu:24.04 AS builder-base

# Avoid prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    ninja-build \
    pkg-config \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# -----------------------------------------------------------------------------
# Stage 2: Builder
# -----------------------------------------------------------------------------
FROM builder-base AS builder

# Build arguments
ARG BUILD_TYPE=Release
ARG ENABLE_TESTS=ON
ARG ENABLE_LTO=ON
ARG SANITIZERS=OFF
ARG COVERAGE=OFF

# Set working directory
WORKDIR /src

# Copy source files
COPY CMakeLists.txt ./
COPY cmake/ ./cmake/
COPY include/ ./include/
COPY src/ ./src/
COPY tests/ ./tests/
COPY examples/ ./examples/

# Create build directory and configure
RUN mkdir -p build && cd build && \
    cmake .. \
        -GNinja \
        -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
        -DCMAKE_INSTALL_PREFIX=/app \
        -DDCF_BUILD_SHARED=ON \
        -DDCF_BUILD_STATIC=ON \
        -DDCF_BUILD_TESTS=${ENABLE_TESTS} \
        -DDCF_BUILD_EXAMPLES=ON \
        -DDCF_ENABLE_LTO=${ENABLE_LTO} \
        -DDCF_ENABLE_SANITIZERS=${SANITIZERS} \
        -DDCF_ENABLE_COVERAGE=${COVERAGE}

# Build
RUN cd build && ninja

# Run tests if enabled
RUN if [ "${ENABLE_TESTS}" = "ON" ]; then \
        cd build && ctest --output-on-failure; \
    fi

# Install
RUN cd build && ninja install

# -----------------------------------------------------------------------------
# Stage 3: Test runner (optional, for CI)
# -----------------------------------------------------------------------------
FROM builder AS test-runner

WORKDIR /src/build

# Default command runs tests
CMD ["ctest", "--output-on-failure", "-V"]

# -----------------------------------------------------------------------------
# Stage 4: Coverage reporter (optional)
# -----------------------------------------------------------------------------
FROM builder-base AS coverage-builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    lcov \
    gcovr \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src

COPY --from=builder /src /src

RUN mkdir -p build && cd build && \
    cmake .. \
        -GNinja \
        -DCMAKE_BUILD_TYPE=Debug \
        -DDCF_BUILD_SHARED=OFF \
        -DDCF_BUILD_STATIC=ON \
        -DDCF_BUILD_TESTS=ON \
        -DDCF_ENABLE_COVERAGE=ON && \
    ninja && \
    ctest --output-on-failure && \
    lcov --capture --directory . --output-file coverage.info && \
    lcov --remove coverage.info '/usr/*' --output-file coverage.info && \
    genhtml coverage.info --output-directory /coverage-report

FROM scratch AS coverage
COPY --from=coverage-builder /coverage-report /coverage-report

# -----------------------------------------------------------------------------
# Stage 5: Runtime (minimal production image)
# -----------------------------------------------------------------------------
FROM ubuntu:24.04 AS runtime

# Install only runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpthread-stubs0-dev \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Create non-root user
RUN useradd -m -s /bin/bash dcf

# Copy installed files from builder
COPY --from=builder /app /app

# Set library path
ENV LD_LIBRARY_PATH=/app/lib

# Set working directory
WORKDIR /app

# Switch to non-root user
USER dcf

# Default command
CMD ["/app/bin/example_basic"]

# -----------------------------------------------------------------------------
# Stage 6: Development image
# -----------------------------------------------------------------------------
FROM builder-base AS dev

# Install additional development tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    gdb \
    valgrind \
    strace \
    ltrace \
    clang \
    clang-tools \
    clang-format \
    clang-tidy \
    cppcheck \
    lcov \
    gcovr \
    doxygen \
    graphviz \
    vim \
    less \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create workspace
WORKDIR /workspace

# Set environment for development
ENV CC=gcc
ENV CXX=g++
ENV CMAKE_GENERATOR=Ninja
ENV ASAN_OPTIONS=detect_leaks=1
ENV UBSAN_OPTIONS=print_stacktrace=1

# Add helper scripts
COPY <<'EOF' /usr/local/bin/dcf-build
#!/bin/bash
set -e
BUILD_TYPE=${1:-Debug}
mkdir -p /workspace/build
cd /workspace/build
cmake /workspace \
    -GNinja \
    -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
    -DDCF_BUILD_TESTS=ON \
    -DDCF_BUILD_EXAMPLES=ON
ninja
EOF

COPY <<'EOF' /usr/local/bin/dcf-test
#!/bin/bash
set -e
cd /workspace/build
ctest --output-on-failure "$@"
EOF

COPY <<'EOF' /usr/local/bin/dcf-format
#!/bin/bash
find /workspace/include /workspace/src -name '*.c' -o -name '*.h' | \
    xargs clang-format -i
echo "Formatted all source files"
EOF

COPY <<'EOF' /usr/local/bin/dcf-check
#!/bin/bash
cppcheck --enable=all --error-exitcode=1 \
    --suppress=missingIncludeSystem \
    --suppress=unusedFunction \
    -I /workspace/include /workspace/src
EOF

RUN chmod +x /usr/local/bin/dcf-*

# Entry point
CMD ["bash"]

# -----------------------------------------------------------------------------
# Stage 7: Static analysis image
# -----------------------------------------------------------------------------
FROM builder-base AS static-analysis

RUN apt-get update && apt-get install -y --no-install-recommends \
    clang \
    clang-tools \
    clang-tidy \
    cppcheck \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src

COPY --from=builder /src /src

# Run static analysis
RUN cppcheck --enable=all --error-exitcode=0 \
    --suppress=missingIncludeSystem \
    --suppress=unusedFunction \
    --xml --xml-version=2 \
    -I include src 2> /cppcheck-report.xml

RUN cd build && \
    cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .. && \
    run-clang-tidy -p . ../src/*.c > /clang-tidy-report.txt 2>&1 || true

FROM scratch AS analysis-reports
COPY --from=static-analysis /cppcheck-report.xml /
COPY --from=static-analysis /clang-tidy-report.txt /

# -----------------------------------------------------------------------------
# Default target: runtime
# -----------------------------------------------------------------------------
FROM runtime
