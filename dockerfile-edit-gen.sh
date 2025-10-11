#!/bin/bash

# Bash script to edit and generate a customized Dockerfile for DCF mono repo SDKs.
# Usage: ./dcf-docker-gen.sh [options]
# Options:
#   -h, --help: Show this help message
#   -o FILE: Output file (default: Dockerfile.generated)
# Interactive mode if no options provided.

# Default Dockerfile template based on DCF repo needs
TEMPLATE=$(cat << 'EOF'
# Use Ubuntu 22.04 as base for multi-language support
FROM ubuntu:22.04

# Set non-interactive mode for apt
ENV DEBIAN_FRONTEND=noninteractive

# Install core tools and dependencies
RUN apt-get update && apt-get install -y \
    git cmake build-essential libprotobuf-dev protobuf-compiler libgrpc++-dev grpc-tools \
    python3 python3-pip perl cpanminus libncurses5-dev libuuid1 uuid-dev libjson-c-dev \
    golang-go rustc cargo nodejs npm sbcl quicklisp \
    openjdk-17-jdk curl unzip wget \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install language-specific dependencies
# Python
RUN pip3 install protobuf grpcio grpcio-tools importlib sphinx myst-parser pytest

# Perl (CPAN modules)
RUN cpanm JSON IO::Socket::INET Getopt::Long Curses::UI Google::ProtocolBuffers::Dynamic Grpc::XS Module::Pluggable --notest

# Node.js
RUN npm install -g grpc protobufjs

# Go (already installed, but ensure paths)
ENV GOPATH=/go
ENV PATH=$PATH:/go/bin

# Rust (already via apt, but update Cargo)
RUN cargo install tonic-build prost-build

# Lisp (Quicklisp setup)
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:quickload "cl-protobufs")' --eval '(ql:quickload "cl-grpc")' --eval '(ql:quickload "cffi")' --quit && \
    rm quicklisp.lisp

# Android (basic SDK tools; mount Android Studio if needed)
ENV ANDROID_SDK_ROOT=/opt/android-sdk
RUN mkdir -p $ANDROID_SDK_ROOT/cmdline-tools && \
    wget https://dl.google.com/android/repository/commandlinetools-linux-9477386_latest.zip -O /tmp/tools.zip && \
    unzip /tmp/tools.zip -d $ANDROID_SDK_ROOT/cmdline-tools && \
    mv $ANDROID_SDK_ROOT/cmdline-tools/cmdline-tools $ANDROID_SDK_ROOT/cmdline-tools/latest && \
    rm /tmp/tools.zip && \
    yes | $ANDROID_SDK_ROOT/cmdline-tools/latest/bin/sdkmanager --licenses && \
    $ANDROID_SDK_ROOT/cmdline-tools/latest/bin/sdkmanager "platform-tools" "platforms;android-34" "build-tools;34.0.0"

# Clone the repo with submodules
WORKDIR /app
RUN git clone --recurse-submodules https://github.com/ALH477/DeMoD-Communication-Framework.git .

# Generate Protobuf/gRPC bindings (adjust paths as per repo)
RUN mkdir -p python/dcf cpp/src go/src nodejs/src perl/lib rust/src lisp/src && \
    protoc --python_out=python/dcf --grpc_python_out=python/dcf messages.proto services.proto && \
    protoc --cpp_out=cpp/src --grpc_out=cpp/src messages.proto services.proto && \
    protoc --go_out=go/src --go-grpc_out=go/src messages.proto services.proto && \
    protoc --js_out=import_style=commonjs:nodejs/src --grpc-web_out=import_style=commonjs,mode=grpcwebtext:nodejs/src messages.proto services.proto && \
    protoc --perl_out=perl/lib messages.proto services.proto  # Grpc::XS handles gRPC separately && \
    # Rust uses build.rs, so defer to cargo build && \
    # Lisp: Use cl-protobufs for generation if needed (manual load in SBCL)

# Build SDKs
# C SDK
RUN cd c_sdk && mkdir build && cd build && cmake .. && make

# StreamDB (Rust, for Lisp integration)
RUN cd streamdb && cargo build --release && cp target/release/libstreamdb.so /usr/local/lib/

# Python (install reqs)
RUN pip3 install -r python/requirements.txt

# Go
RUN cd go && go build ./...

# Rust
RUN cd rust && cargo build

# Node.js
RUN cd nodejs && npm install

# Perl (no build, deps already installed)

# Lisp (load for build/test)
RUN sbcl --load lisp/src/d-lisp.lisp --quit

# Android (Gradle build; assumes gradlew in repo)
RUN cd android && ./gradlew build

# iOS (limited; full build needs macOS/Xcode - this installs deps only)
RUN swift package resolve  # If using Swift Package Manager

# Build documentation
RUN cd docs && pip3 install -r requirements.txt && make html

# Set entrypoint for interactive shell (e.g., run tests or examples)
ENTRYPOINT ["/bin/bash"]
EOF
)

# Variables for editable sections
BASE_IMAGE="ubuntu:22.04"
CORE_DEPS="git cmake build-essential libprotobuf-dev protobuf-compiler libgrpc++-dev grpc-tools python3 python3-pip perl cpanminus libncurses5-dev libuuid1 uuid-dev libjson-c-dev golang-go rustc cargo nodejs npm sbcl quicklisp openjdk-17-jdk curl unzip wget"
PYTHON_PIPS="protobuf grpcio grpcio-tools importlib sphinx myst-parser pytest"
PERL_CPANS="JSON IO::Socket::INET Getopt::Long Curses::UI Google::ProtocolBuffers::Dynamic Grpc::XS Module::Pluggable"
NODE_GLOBALS="grpc protobufjs"
CARGO_INSTALLS="tonic-build prost-build"
ANDROID_VERSION="34"
REPO_URL="https://github.com/ALH477/DeMoD-Communication-Framework.git"
OUTPUT_FILE="Dockerfile.generated"

# Function to generate Dockerfile from template with vars
generate_dockerfile() {
  echo "$TEMPLATE" | sed \
    -e "s/FROM ubuntu:22.04/FROM $BASE_IMAGE/" \
    -e "s/apt-get install -y [^&]* \&\&/apt-get install -y $CORE_DEPS \&\&/" \
    -e "s/pip3 install [^$]*/pip3 install $PYTHON_PIPS/" \
    -e "s/cpanm [^ ]* /cpanm $PERL_CPANS /" \
    -e "s/npm install -g [^$]*/npm install -g $NODE_GLOBALS/" \
    -e "s/cargo install [^$]*/cargo install $CARGO_INSTALLS/" \
    -e "s/platforms;android-34/platforms;android-$ANDROID_VERSION/" \
    -e "s/build-tools;34.0.0/build-tools;$ANDROID_VERSION.0.0/" \
    -e "s|git clone --recurse-submodules https://github.com/ALH477/DeMoD-Communication-Framework.git .|git clone --recurse-submodules $REPO_URL .|"
}

# Help message
show_help() {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  -h, --help             Show this help"
  echo "  -o FILE                Output file (default: $OUTPUT_FILE)"
  echo "Interactive mode: Run without options to edit sections via prompts."
  exit 0
}

# Parse options
while [[ $# -gt 0 ]]; do
  case $1 in
    -h|--help) show_help ;;
    -o) OUTPUT_FILE="$2"; shift ;;
    *) echo "Unknown option: $1"; show_help ;;
  esac
  shift
done

# Interactive editing
echo "Entering interactive mode for DCF Dockerfile customization..."
read -p "Base image (default: $BASE_IMAGE): " input
BASE_IMAGE=${input:-$BASE_IMAGE}

read -p "Core apt dependencies (space-separated, default: $CORE_DEPS): " input
CORE_DEPS=${input:-$CORE_DEPS}

read -p "Python pip installs (space-separated, default: $PYTHON_PIPS): " input
PYTHON_PIPS=${input:-$PYTHON_PIPS}

read -p "Perl CPAN modules (space-separated, default: $PERL_CPANS): " input
PERL_CPANS=${input:-$PERL_CPANS}

read -p "Node global installs (space-separated, default: $NODE_GLOBALS): " input
NODE_GLOBALS=${input:-$NODE_GLOBALS}

read -p "Cargo installs (space-separated, default: $CARGO_INSTALLS): " input
CARGO_INSTALLS=${input:-$CARGO_INSTALLS}

read -p "Android API version (default: $ANDROID_VERSION): " input
ANDROID_VERSION=${input:-$ANDROID_VERSION}

read -p "Repo URL (default: $REPO_URL): " input
REPO_URL=${input:-$REPO_URL}

# Generate and save
generate_dockerfile > "$OUTPUT_FILE"
echo "Generated customized Dockerfile at $OUTPUT_FILE"
