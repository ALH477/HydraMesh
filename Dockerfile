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
