#!/bin/bash

set -euo pipefail  # Enable strict mode: exit on error, unset variables, and pipe failures

# Global variable to track errors
ERRORS=0
ERROR_LOG=()

# Function to log errors without exiting
log_error() {
    local msg="$1"
    echo "Error: $msg" >&2
    ERROR_LOG+=("$msg")
    ((ERRORS++))
}

# Function to report summary of errors at the end
report_errors() {
    if [ "$ERRORS" -gt 0 ]; then
        echo "Installation completed with $ERRORS errors:" >&2
        for err in "${ERROR_LOG[@]}"; do
            echo "- $err" >&2
        done
        echo "Some packages could not be installed. Check your system configuration or install them manually." >&2
    else
        echo "Installation completed successfully with no errors."
    fi
}

# Function to detect distro
detect_distro() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        DISTRO="${ID_LIKE:-$ID}"
    else
        log_error "/etc/os-release not found. Unsupported distro."
        exit 1  # Exit here as distro detection is critical
    fi
    # Normalize distro names
    case "$DISTRO" in
        debian|ubuntu)
            DISTRO="debian"
            ;;
        arch|manjaro)
            DISTRO="arch"
            ;;
        fedora|rhel|centos)
            DISTRO="fedora"
            ;;
        *)
            log_error "Unsupported distro: $DISTRO."
            exit 1  # Exit here as unsupported distro
            ;;
    esac
    echo "Detected distro family: $DISTRO"
}

# Function to install system packages with distro-specific commands
install_system_packages() {
    echo "Updating package index..."
    case "$DISTRO" in
        debian)
            sudo apt update -y || log_error "apt update failed."
            sudo apt install -y --no-install-recommends \
                git cmake make protobuf-compiler libprotobuf-dev libgrpc++-dev libprotobuf-c-dev uuid-dev libcjson-dev libncurses-dev libc6-dev clang-format valgrind python3 python3-pip perl libapp-cpanminus-perl nodejs npm golang-go libssl-dev sbcl \
                || log_error "apt install failed for some packages."
            ;;
        arch)
            sudo pacman -Syu --noconfirm --needed \
                git cmake make protobuf grpc protobuf-c util-linux cjson ncurses glibc clang valgrind python python-pip perl perl-app-cpanminus nodejs npm go openssl sbcl \
                || log_error "pacman install failed for some packages."
            ;;
        fedora)
            sudo dnf update -y || log_error "dnf update failed."
            sudo dnf install -y \
                git cmake make protobuf-compiler protobuf-devel grpc-devel protobuf-c-devel libuuid-devel json-c-devel ncurses-devel glibc-devel clang-tools-extra valgrind python3 python3-pip perl perl-App-cpanminus nodejs npm golang openssl-devel sbcl \
                || log_error "dnf install failed for some packages."
            ;;
    esac
    echo "System packages installation attempted."
}

# Function to install language-specific dependencies
install_lang_deps() {
    echo "Installing Perl CPAN modules..."
    cpanm --quiet --notest JSON IO::Socket::INET Getopt::Long Curses::UI Google::ProtocolBuffers::Dynamic Grpc::XS Module::Pluggable \
        || log_error "cpanm failed to install some Perl modules."

    echo "Installing Python pip modules..."
    python3 -m pip install --user --upgrade protobuf grpcio grpcio-tools \
        || log_error "pip install failed for some Python modules."

    echo "Installing Node.js npm modules globally..."
    sudo npm install -g @grpc/grpc-js @grpc/proto-loader \
        || log_error "npm install failed for some Node.js modules."

    echo "Installing Go modules..."
    go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest \
        && go install google.golang.org/protobuf/cmd/protoc-gen-go@latest \
        || log_error "go install failed for some Go modules."

    echo "Setting up Rust..."
    if ! command -v rustup &> /dev/null; then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable --profile default \
            || log_error "rustup installation failed."
        source "$HOME/.cargo/env" || log_error "Failed to source Rust environment."
    else
        rustup update stable || log_error "rustup update failed."
    fi
    cargo install tonic-build prost-build \
        || log_error "cargo install failed for some Rust crates."

    echo "Installing Quicklisp for Common Lisp..."
    if command -v sbcl &> /dev/null; then
        curl -O https://beta.quicklisp.org/quicklisp.lisp || log_error "Failed to download quicklisp.lisp."
        sbcl --no-userinit --no-sysinit --non-interactive --load quicklisp.lisp \
            --eval '(quicklisp-quickstart:install)' \
            --eval '(ql:add-to-init-file)' \
            --quit || log_error "SBCL failed to install Quicklisp."
        rm -f quicklisp.lisp || true  # Cleanup, ignore failure
    else
        log_error "SBCL not found. Quicklisp installation skipped."
    fi

    echo "Language-specific dependencies installation attempted."
}

# Main execution
detect_distro

read -p "Install system packages? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    install_system_packages
fi

read -p "Install language-specific dependencies? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    install_lang_deps
fi

report_errors

echo "Installation complete. Clone the DCF repo: git clone --recurse-submodules https://github.com/ALH477/DeMoD-Communication-Framework.git"
echo "Then follow build instructions for the SDKs, including potential Common Lisp SDK."
