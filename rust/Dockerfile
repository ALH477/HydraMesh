# ============================================================================
# DCF Rust SDK - Hardened Production Dockerfile
# ============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Builder
# -----------------------------------------------------------------------------
FROM rust:1.83-alpine3.21 AS builder

# Install build dependencies
# protobuf-dev is required for tonic-build (gRPC)
RUN apk add --no-cache \
    musl-dev \
    openssl-dev \
    openssl-libs-static \
    pkgconfig \
    protobuf-dev \
    git \
    && rm -rf /var/cache/apk/*

# Create non-root build user
RUN addgroup -g 1000 builder && \
    adduser -u 1000 -G builder -s /bin/sh -D builder

WORKDIR /build

# Copy manifests first to cache dependency downloads
COPY --chown=builder:builder Cargo.toml Cargo.lock* ./

# Create dummy source files to force cargo to build dependencies only
# This layer will be cached unless Cargo.toml changes
RUN mkdir -p src && \
    echo 'fn main() { println!("dummy"); }' > src/main.rs && \
    echo 'pub fn dummy() {}' > src/lib.rs

# Build dependencies (this takes time, so we cache it)
RUN cargo build --release

# Remove dummy sources
RUN rm -rf src

# Copy actual source code
# This layer invalidates whenever you change lib.rs or main.rs
COPY --chown=builder:builder src/ src/
COPY --chown=builder:builder proto/ proto/
COPY --chown=builder:builder build.rs* ./

# Touch files to ensure the compiler notices the timestamp change
RUN touch src/main.rs src/lib.rs

# Build the actual application
RUN cargo build --release --locked

# Strip debug symbols to reduce binary size
RUN strip target/release/dcf

# -----------------------------------------------------------------------------
# Stage 2: Hardened Runtime (Alpine)
# Balances security with debuggability (has a shell for troubleshooting)
# -----------------------------------------------------------------------------
FROM alpine:3.21 AS runtime

# Security hardening & Runtime dependencies
RUN apk add --no-cache \
    ca-certificates \
    libgcc \
    # Clean up apk cache
    && rm -rf /var/cache/apk/* \
    # Remove unnecessary temporary directories
    && rm -rf /tmp/* /var/tmp/*

# Create a dedicated non-root user for the application
RUN addgroup -g 10001 dcf && \
    adduser -u 10001 -G dcf -s /sbin/nologin -D -H dcf

# Create standard directories with correct permissions
RUN mkdir -p /etc/dcf /var/lib/dcf /var/log/dcf && \
    chown -R dcf:dcf /var/lib/dcf /var/log/dcf

# Copy the compiled binary from the builder stage
COPY --from=builder --chown=dcf:dcf /build/target/release/dcf /usr/local/bin/dcf

# Copy the default configuration
COPY --from=builder /build/dcf_config.toml* /etc/dcf/

# Set strict permissions on the binary
RUN chmod 755 /usr/local/bin/dcf && \
    chmod 750 /etc/dcf /var/lib/dcf /var/log/dcf

# Switch to non-root user
USER dcf:dcf

# Set working directory
WORKDIR /var/lib/dcf

# Environment Variables
ENV DCF_CONFIG=/etc/dcf/dcf_config.toml
# Default log level is info (matches your code update)
ENV RUST_LOG=info 

# Expose necessary ports
# 7777/udp: Game Traffic (HydraMesh)
# 50051/tcp: gRPC Management
EXPOSE 7777/udp 50051/tcp

# Health check ensures the node is responsive
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
    CMD /usr/local/bin/dcf status || exit 1

# Entry point
ENTRYPOINT ["/usr/local/bin/dcf"]
CMD ["start"]
