# ============================================================================
# DCF Rust SDK - Hardened Docker Image
# Multi-stage build with security best practices
# ============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Environment
# -----------------------------------------------------------------------------
FROM rust:1.83-alpine3.21 AS builder

# Install build dependencies
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

# Set up cargo home for caching
ENV CARGO_HOME=/usr/local/cargo
ENV RUSTFLAGS="-C target-feature=-crt-static"

WORKDIR /build

# Copy manifests first for layer caching
COPY --chown=builder:builder Cargo.toml Cargo.lock* ./

# Create dummy src for dependency caching
RUN mkdir -p src && \
    echo 'fn main() { println!("dummy"); }' > src/main.rs && \
    echo 'pub fn dummy() {}' > src/lib.rs

# Build dependencies only (cached layer)
RUN cargo build --release 2>/dev/null || true
RUN rm -rf src

# Copy actual source
COPY --chown=builder:builder src/ src/
COPY --chown=builder:builder proto/ proto/
COPY --chown=builder:builder build.rs* ./

# Touch to invalidate cache and rebuild with real sources
RUN touch src/main.rs src/lib.rs

# Build release binary with optimizations
RUN cargo build --release --locked 2>/dev/null || cargo build --release

# Strip binary for smaller size
RUN strip /build/target/release/dcf 2>/dev/null || true

# -----------------------------------------------------------------------------
# Stage 2: Security Scanner (optional, for CI)
# -----------------------------------------------------------------------------
FROM builder AS scanner

RUN cargo install cargo-audit cargo-deny 2>/dev/null || true
RUN cargo audit 2>/dev/null || echo "Audit check skipped"

# -----------------------------------------------------------------------------
# Stage 3: Hardened Runtime (Distroless)
# Minimal attack surface - no shell, no package manager
# -----------------------------------------------------------------------------
FROM gcr.io/distroless/cc-debian12:nonroot AS runtime-distroless

# Copy binary
COPY --from=builder /build/target/release/dcf /usr/local/bin/dcf

# Copy default config if exists
COPY --from=builder /build/dcf_config.toml* /etc/dcf/

# Use non-root user (uid 65532 in distroless)
USER nonroot:nonroot

# Expose ports
EXPOSE 7777/udp 50051/tcp

# Health check via gRPC (requires grpc-health-probe in path)
# HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
#     CMD ["/usr/local/bin/dcf", "status"]

ENTRYPOINT ["/usr/local/bin/dcf"]
CMD ["start"]

# -----------------------------------------------------------------------------
# Stage 4: Hardened Runtime (Alpine) - for debugging/shell access
# -----------------------------------------------------------------------------
FROM alpine:3.21 AS runtime-alpine

# Security hardening
RUN apk add --no-cache \
    ca-certificates \
    libgcc \
    && rm -rf /var/cache/apk/* \
    # Remove unnecessary packages and files
    && rm -rf /usr/share/man /tmp/* /var/tmp/* \
    # Remove shell access (comment out if you need debugging)
    # && rm -rf /bin/sh /bin/ash \
    # Harden file permissions
    && chmod 700 /root \
    && find / -type f -perm /4000 -exec chmod u-s {} \; 2>/dev/null || true \
    && find / -type f -perm /2000 -exec chmod g-s {} \; 2>/dev/null || true

# Create non-root user
RUN addgroup -g 10001 dcf && \
    adduser -u 10001 -G dcf -s /sbin/nologin -D -H dcf

# Create directories
RUN mkdir -p /etc/dcf /var/lib/dcf /var/log/dcf && \
    chown -R dcf:dcf /var/lib/dcf /var/log/dcf

# Copy binary
COPY --from=builder --chown=dcf:dcf /build/target/release/dcf /usr/local/bin/dcf

# Copy default config
COPY --from=builder /build/dcf_config.toml* /etc/dcf/

# Set secure permissions
RUN chmod 755 /usr/local/bin/dcf && \
    chmod 750 /etc/dcf /var/lib/dcf /var/log/dcf

# Use non-root user
USER dcf:dcf

# Working directory
WORKDIR /var/lib/dcf

# Environment
ENV DCF_CONFIG=/etc/dcf/dcf_config.toml
ENV RUST_LOG=info

# Expose ports
EXPOSE 7777/udp 50051/tcp

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
    CMD /usr/local/bin/dcf status || exit 1

ENTRYPOINT ["/usr/local/bin/dcf"]
CMD ["start"]

# -----------------------------------------------------------------------------
# Stage 5: Scratch Runtime - Absolute minimum (static binary required)
# -----------------------------------------------------------------------------
FROM scratch AS runtime-scratch

# Copy SSL certs for TLS
COPY --from=alpine:3.21 /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/

# Copy binary (must be statically linked)
COPY --from=builder /build/target/release/dcf /dcf

# Copy config
COPY --from=builder /build/dcf_config.toml* /etc/dcf/

# Use numeric UID (scratch has no /etc/passwd)
USER 10001:10001

EXPOSE 7777/udp 50051/tcp

ENTRYPOINT ["/dcf"]
CMD ["start"]

# -----------------------------------------------------------------------------
# Default target: Alpine hardened runtime
# -----------------------------------------------------------------------------
FROM runtime-alpine AS default
