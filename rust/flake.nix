{
  description = "DCF Remote Node - Professional Grade Systems Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        # 1. Clean Source Filter (Prevents unnecessary rebuilds)
        src = pkgs.lib.cleanSourceWith {
          src = ./.;
          filter = path: type:
            let base = baseNameOf path; in
            # Ignore target, git, and build artifacts
            !(type == "directory" && base == "target") &&
            !(type == "directory" && base == ".git") &&
            base != "flake.lock";
        };

        cargoToml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
        version = cargoToml.package.version;
        pname = "dcf-node";

        # 2. The Fixed Rust Build
        dcf-node = pkgs.rustPlatform.buildRustPackage {
          inherit pname version src;
          cargoLock.lockFile = ./Cargo.lock;

          # CRITICAL FIX: Add protobuf so tonic-build can run
          nativeBuildInputs = with pkgs; [ 
            pkg-config 
            protobuf  # Provides 'protoc'
          ];
          
          buildInputs = [ pkgs.openssl ];

          # CRITICAL FIX: Trick build.rs into writing to the Nix build temp dir
          # instead of trying to write to the read-only 'src/generated'.
          # We delete the hardcoded out_dir line before compiling.
          preConfigure = ''
            sed -i '/.out_dir("src\/generated")/d' build.rs
          '';

          # Disable tests that might require network access (sandbox violation)
          doCheck = false;
        };

        # 3. Layered Docker Image (Automatic dependency resolution)
        dockerImage = pkgs.dockerTools.streamLayeredImage {
          name = "alh477/${pname}";
          tag = version;
          
          # Nix automatically detects libraries (glibc, openssl) referenced 
          # by the binary and adds them to the image closure.
          contents = [ 
            pkgs.cacert 
            pkgs.tzdata
            dcf-node 
          ];
          
          config = {
            User = "1000:1000";
            ExposedPorts = {
              "7777/udp" = {};
              "50051/tcp" = {};
            };
            Env = [
              "DCF_CONFIG=/etc/dcf/config.toml"
              "RUST_LOG=info"
            ];
            Entrypoint = [ "${dcf-node}/bin/dcf" ]; # Dockerfile uses "dcf" not "dcf-node"
          };
        };

        # 4. UX Scripts (Unchanged)
        mkConfigScript = pkgs.writeShellScriptBin "dcf-init-config" ''
          CONFIG_PATH="./config.toml"
          if [ -f "$CONFIG_PATH" ]; then
            echo "ℹ️  Config file already exists."
          else
            echo "🚀 Generating production config..."
            cat > "$CONFIG_PATH" <<EOF
[node]
id = "node-$(hostname)-01"
hub_url = "https://dcf.demod.ltd"
[network]
bind_addr = "0.0.0.0"
bind_port = 7777
EOF
            echo "✅ Created $CONFIG_PATH"
          fi
          $EDITOR "$CONFIG_PATH"
        '';

        mkDeployScript = pkgs.writeShellScriptBin "dcf-gen-deploy" ''
          cat > docker-compose.yml <<EOF
services:
  dcf-node:
    image: alh477/${pname}:${version}
    restart: unless-stopped
    cap_drop: [ALL]
    cap_add: [SYS_NICE, NET_RAW, IPC_LOCK]
    read_only: true
    security_opt: [no-new-privileges:true]
    ulimits:
      rtprio: { soft: 99, hard: 99 }
      memlock: { soft: -1, hard: -1 }
    ports:
      - "7777:7777/udp"
      - "50051:50051/tcp"
    volumes:
      - ./config.toml:/etc/dcf/config.toml:ro
EOF
          echo "✅ Generated hardened docker-compose.yml"
        '';

      in
      {
        packages.default = dcf-node;
        packages.container = dockerImage;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            (rust-bin.stable.latest.default.override {
              extensions = [ "rust-src" "rust-analyzer" ];
            })
            pkg-config
            openssl
            protobuf # Available in shell for local 'cargo build'
            mkConfigScript
            mkDeployScript
            docker-compose
          ];
          
          # Fix for local cargo builds to find protoc
          shellHook = ''
            export PROTOC="${pkgs.protobuf}/bin/protoc"
            export PROTOC_INCLUDE="${pkgs.protobuf}/include"
            
            echo "🛡️  DeMoD DCF-Node Environment Active"
          '';
        };
      }
    );
}
