# ============================================================================
# HydraMesh Lisp - Hardened Docker Image
# Multi-stage build with SBCL and security best practices
# ============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Environment with Quicklisp
# -----------------------------------------------------------------------------
FROM fukamachi/sbcl:2.4.11-debian AS builder

# Install build dependencies
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libssl-dev \
    libffi-dev \
    zlib1g-dev \
    git \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy source files
COPY src/ /build/src/
COPY hydramesh.asd /build/
COPY config/ /build/config/

# The fukamachi/sbcl image uses Roswell - Quicklisp is already available via ros
# Install dependencies using ros/quicklisp
RUN ros -e '(ql:quickload (quote (:cffi :uuid :usocket :bordeaux-threads :log4cl :trivial-backtrace :flexi-streams :fiveam :ieee-floats :cl-json)) :silent t)' -e '(quit)'

# Build the executable core image using ros
RUN ros -e '(ql:quickload (quote (:cffi :uuid :usocket :bordeaux-threads :log4cl :trivial-backtrace :flexi-streams :fiveam :ieee-floats :cl-json)) :silent t)' \
    -l /build/src/hydramesh.lisp \
    -e '(in-package :d-lisp)' \
    -e '(sb-ext:save-lisp-and-die "/build/hydramesh" :executable t :toplevel (lambda () (apply (function main) (rest sb-ext:*posix-argv*))) :compression 9)'

# -----------------------------------------------------------------------------
# Stage 2: Hardened Runtime
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Security hardening
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libssl3 \
    libffi8 \
    zlib1g \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /usr/share/man /tmp/* /var/tmp/* \
    && chmod 700 /root \
    && find / -type f -perm /4000 -exec chmod u-s {} \; 2>/dev/null || true \
    && find / -type f -perm /2000 -exec chmod g-s {} \; 2>/dev/null || true

# Create non-root user
RUN groupadd -g 10001 hydramesh && \
    useradd -u 10001 -g hydramesh -s /usr/sbin/nologin -M -d /var/lib/hydramesh hydramesh

# Create directories
RUN mkdir -p /etc/hydramesh /var/lib/hydramesh /var/log/hydramesh && \
    chown -R hydramesh:hydramesh /var/lib/hydramesh /var/log/hydramesh

# Copy executable from builder
COPY --from=builder --chown=hydramesh:hydramesh /build/hydramesh /usr/local/bin/hydramesh

# Copy config
COPY --chown=hydramesh:hydramesh config/ /etc/hydramesh/

# Set secure permissions
RUN chmod 755 /usr/local/bin/hydramesh && \
    chmod 750 /etc/hydramesh /var/lib/hydramesh /var/log/hydramesh

# Use non-root user
USER hydramesh:hydramesh

# Working directory
WORKDIR /var/lib/hydramesh

# Environment
ENV HYDRAMESH_CONFIG=/etc/hydramesh/config.json
ENV HOME=/var/lib/hydramesh

# Expose ports
EXPOSE 7777/udp 50051/tcp

# Health check
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
    CMD /usr/local/bin/hydramesh status || exit 1

ENTRYPOINT ["/usr/local/bin/hydramesh"]
CMD ["help"]

# -----------------------------------------------------------------------------
# Stage 3: Development Runtime (with SBCL REPL)
# -----------------------------------------------------------------------------
FROM fukamachi/sbcl:2.4.11-debian AS runtime-dev

USER root

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libssl3 \
    libffi8 \
    zlib1g \
    rlwrap \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN groupadd -g 10001 hydramesh && \
    useradd -u 10001 -g hydramesh -m -s /bin/bash -d /home/hydramesh hydramesh

# Copy source
COPY --chown=hydramesh:hydramesh src/ /home/hydramesh/src/

# Create directories
RUN mkdir -p /etc/hydramesh /var/lib/hydramesh /var/log/hydramesh && \
    chown -R hydramesh:hydramesh /var/lib/hydramesh /var/log/hydramesh /home/hydramesh

# Copy config
COPY --chown=hydramesh:hydramesh config/ /etc/hydramesh/

# Pre-install dependencies as root (Roswell cache is system-wide)
RUN ros -e '(ql:quickload (quote (:cffi :uuid :usocket :bordeaux-threads :log4cl :trivial-backtrace :flexi-streams :fiveam :ieee-floats :cl-json)) :silent t)' -e '(quit)'

# Create init script for REPL
RUN printf '#!/bin/bash\nrlwrap ros -e "(ql:quickload (quote (:cffi :uuid :usocket :bordeaux-threads :log4cl :trivial-backtrace :flexi-streams :fiveam :ieee-floats :cl-json)) :silent t)" -l /home/hydramesh/src/hydramesh.lisp -e "(in-package :d-lisp)" "$@"\n' > /usr/local/bin/hydramesh-repl && \
    chmod +x /usr/local/bin/hydramesh-repl

# Use non-root user
USER hydramesh:hydramesh
WORKDIR /home/hydramesh

# Environment
ENV HOME=/home/hydramesh
ENV HYDRAMESH_CONFIG=/etc/hydramesh/config.json

# Expose ports
EXPOSE 7777/udp 50051/tcp

# Default to REPL
CMD ["/usr/local/bin/hydramesh-repl"]

# -----------------------------------------------------------------------------
# Default target
# -----------------------------------------------------------------------------
FROM runtime AS default
