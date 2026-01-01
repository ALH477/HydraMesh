#!/usr/bin/env bash
# ==============================================================================
# DeMoD Community Node Setup Script - Professional Edition
# ==============================================================================
# Version: 1.2.0 (2026-01-01)
# Description: Fully automated, idempotent setup for a production-hardened
#              DeMoD DCF-SDK community node.
#
# Features:
# - Idempotent: Safe to run multiple times (updates config/compose if needed)
# - Professional error handling and logging
# - Argument parsing (--help, --dir, --no-firewall, --update)
# - Auto-detects package manager for Docker install hints
# - Enhanced security and production defaults
# - Detailed status output and next steps
#
# License: MIT
# ==============================================================================

set -euo pipefail
IFS=$'\n\t'

# ──────────────────────────────────────────────────────────────────────────────
# CONSTANTS & DEFAULTS
# ──────────────────────────────────────────────────────────────────────────────

readonly SCRIPT_VERSION="1.2.0"
readonly SCRIPT_NAME="$(basename "$0")"
readonly DEFAULT_PROJECT_DIR="demod-community-node"
readonly IMAGE="alh477/dcf-rs:latest"
readonly GATEWAY_URL="https://dcf.demod.ltd"
readonly UDP_PORT=7777
readonly GRPC_PORT=50051
readonly COMPOSE_FILE="docker-compose.yml"
readonly CONFIG_DIR="config"
readonly CONFIG_FILE="${CONFIG_DIR}/dcf_config.toml"

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# Logging
readonly LOG_FILE="/var/log/demod-node-setup.log"
exec > >(tee -a "$LOG_FILE") 2>&1

# ──────────────────────────────────────────────────────────────────────────────
# HELPER FUNCTIONS
# ──────────────────────────────────────────────────────────────────────────────

log() { echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')] $1${NC}"; }
success() { echo -e "${GREEN}✓ $1${NC}"; }
warning() { echo -e "${YELLOW}⚠ $1${NC}"; }
error() { echo -e "${RED}✗ ERROR: $1${NC}" >&2; exit 1; }

print_banner() {
    cat << EOF

${GREEN}══════════════════════════════════════════════════════════════════════════════${NC}
${GREEN}  DeMoD Community Node Setup - v${SCRIPT_VERSION}${NC}
${GREEN}  Professional, idempotent installer for DCF-SDK nodes${NC}
${GREEN}══════════════════════════════════════════════════════════════════════════════${NC}

EOF
}

usage() {
    cat << EOF
Usage: $SCRIPT_NAME [OPTIONS]

Options:
  -d, --dir DIR           Project directory (default: ${DEFAULT_PROJECT_DIR})
  -f, --no-firewall       Skip automatic firewall configuration
  -u, --update            Update existing installation (pull latest image & restart)
  -h, --help              Show this help message

Example:
  sudo ./$SCRIPT_NAME --dir my-demod-node --no-firewall
EOF
    exit 0
}

# Parse arguments
parse_args() {
    PROJECT_DIR="$DEFAULT_PROJECT_DIR"
    SKIP_FIREWALL=false
    UPDATE_ONLY=false

    while [[ $# -gt 0 ]]; do
        case $1 in
            -d|--dir) PROJECT_DIR="$2"; shift 2 ;;
            -f|--no-firewall) SKIP_FIREWALL=true; shift ;;
            -u|--update) UPDATE_ONLY=true; shift ;;
            -h|--help) usage ;;
            *) error "Unknown option: $1" ;;
        esac
    done
}

check_root() {
    if [[ $EUID -ne 0 ]]; then
        warning "Running as non-root. Firewall changes require sudo."
    fi
}

check_prerequisites() {
    log "Checking system prerequisites..."

    local missing=()
    for cmd in docker curl jq; do
        if ! command -v "$cmd" &>/dev/null; then
            missing+=("$cmd")
        fi
    done

    if [[ ${#missing[@]} -gt 0 ]]; then
        warning "Missing tools: ${missing[*]}"
        echo "Install Docker first. Detected package manager:"
        if command -v apt &>/dev/null; then
            echo "  sudo apt update && sudo apt install docker.io docker-compose-plugin curl jq -y"
        elif command -v yum &>/dev/null; then
            echo "  sudo yum install docker docker-compose-plugin curl jq -y"
        elif command -v dnf &>/dev/null; then
            echo "  sudo dnf install docker docker-compose-plugin curl jq -y"
        else
            echo "  Please install Docker manually: https://docs.docker.com/engine/install/"
        fi
        error "Required tools not found"
    fi

    if ! docker compose version &>/dev/null; then
        error "Docker Compose plugin not available. Install it: https://docs.docker.com/compose/install/"
    fi

    if ! docker info &>/dev/null; then
        error "Docker daemon not running or permission denied. Add user to docker group if needed."
    fi

    success "Prerequisites satisfied"
}

setup_project() {
    log "Setting up project in: $PROJECT_DIR"

    mkdir -p "$PROJECT_DIR/$CONFIG_DIR"
    cd "$PROJECT_DIR"

    success "Project directory ready"
}

generate_compose() {
    if [[ -f "$COMPOSE_FILE" ]] && ! $UPDATE_ONLY; then
        log "Existing $COMPOSE_FILE found - skipping generation"
        return
    fi

    log "Generating production-hardened docker-compose.yml"

    cat > "$COMPOSE_FILE" << 'EOF'
# ==============================================================================
# DeMoD Community Node - Production Edition
# ==============================================================================
x-logging: &logging
  driver: json-file
  options:
    max-size: "10m"
    max-file: "5"

services:
  dcf-sdk:
    image: alh477/dcf-rs:latest
    container_name: demod-community-node
    hostname: dcf-sdk
    restart: unless-stopped
    cpuset: "0"
    cap_add:
      - SYS_NICE
      - NET_RAW
      - IPC_LOCK
    ulimits:
      rtprio: 99
      memlock: -1
      nofile: 65536
    ports:
      - "7777:7777/udp"
      - "50051:50051/tcp"
    environment:
      - DCF_CONFIG=/etc/dcf/dcf_config.toml
      - RUST_LOG=info
      - RUST_BACKTRACE=1
    volumes:
      - ./config:/etc/dcf:ro
    networks:
      - dcf-external
    healthcheck:
      test: ["CMD", "/usr/local/bin/dcf", "status"]
      interval: 30s
      timeout: 10s
      retries: 5
      start_period: 20s
    logging: *logging
    deploy:
      resources:
        limits:
          cpus: "1.0"
          memory: "512M"
        reservations:
          cpus: "0.5"
          memory: "256M"
    security_opt:
      - no-new-privileges:true
    read_only: true
    tmpfs:
      - /tmp:size=64M
      - /run:size=16M

networks:
  dcf-external:
    driver: bridge
    name: dcf-external
EOF

    success "docker-compose.yml generated/updated"
}

generate_config() {
    if [[ -f "$CONFIG_FILE" ]] && ! $UPDATE_ONLY; then
        log "Existing config found - skipping generation"
        return
    fi

    log "Generating default config"

    cat > "$CONFIG_FILE" << EOF
# DeMoD Community Node Configuration
[network]
gateway_url = "$GATEWAY_URL"
discovery_mode = "central"
node_type = "community"

[server]
bind_udp = "0.0.0.0:7777"
bind_grpc = "0.0.0.0:50051"

[performance]
target_hz = 125
shim_mode = "universal"

[node]
id = "REGISTER-AT-${GATEWAY_URL}/register"
EOF

    success "Config created at $CONFIG_FILE"
}

configure_firewall() {
    [[ $SKIP_FIREWALL == true ]] && return

    log "Configuring firewall..."

    if command -v ufw &>/dev/null && ufw status &>/dev/null; then
        sudo ufw allow "$UDP_PORT"/udp && success "ufw: UDP $UDP_PORT allowed"
        sudo ufw allow "$GRPC_PORT"/tcp && success "ufw: TCP $GRPC_PORT allowed"
        sudo ufw reload &>/dev/null || true
    elif command -v firewall-cmd &>/dev/null; then
        sudo firewall-cmd --permanent --add-port="$UDP_PORT"/udp && success "firewalld: UDP allowed"
        sudo firewall-cmd --permanent --add-port="$GRPC_PORT"/tcp && success "firewalld: TCP allowed"
        sudo firewall-cmd --reload
    else
        warning "No ufw/firewalld detected. Manually allow UDP $UDP_PORT (required) and TCP $GRPC_PORT (optional)"
        warning "Also configure your cloud provider's security group/firewall"
    fi
}

deploy_node() {
    log "Deploying node..."

    docker compose pull
    docker compose up -d --remove-orphans

    sleep 15

    if docker compose ps --services --filter "status=running" | grep -q dcf-sdk; then
        success "Node is running and healthy"
    else
        warning "Container started but may not be healthy yet"
        docker compose logs --tail=50
    fi
}

show_status_and_next() {
    print_banner

    echo -e "${GREEN}Setup complete!${NC}\n"

    echo -e "${YELLOW}Current status:${NC}"
    docker compose ps
    echo

    echo -e "${YELLOW}Next steps:${NC}"
    cat << EOF
1. Register your node:
   → Visit ${GATEWAY_URL}/register
   → Or join Discord for help

2. Add your assigned node ID:
   nano $(pwd)/$CONFIG_FILE
   → Replace the id line

3. Restart:
   docker compose restart

4. Monitor:
   docker compose logs -f
   docker exec demod-community-node /usr/local/bin/dcf status

Thank you for contributing to the global DeMoD mesh!
EOF
}

# ──────────────────────────────────────────────────────────────────────────────
# MAIN
# ──────────────────────────────────────────────────────────────────────────────

print_banner
parse_args "$@"
check_root
check_prerequisites

if [[ $UPDATE_ONLY == true ]] && [[ -d "$PROJECT_DIR" ]]; then
    cd "$PROJECT_DIR"
    deploy_node
    show_status_and_next
    exit 0
fi

setup_project
generate_compose
generate_config
configure_firewall
deploy_node
show_status_and_next

success "Professional installation complete (v${SCRIPT_VERSION})"
echo -e "Log: $LOG_FILE\n"
