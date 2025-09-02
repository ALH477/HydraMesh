{
  description = "DeMoD Communications Framework (DCF) Mono Repo SDK Development";

  # Pin to a stable Nixpkgs release for reproducibility; update via `nix flake update`.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Overlays for pinning specific versions of critical dependencies to ensure DCF Protobuf/gRPC interoperability.
        overlays = [
          (self: super: {
            protobuf = super.protobuf.overrideAttrs (old: rec {
              version = "3.21.12";
              src = super.fetchurl {
                url = "https://github.com/protocolbuffers/protobuf/releases/download/v${version}/protobuf-all-${version}.tar.gz";
                sha256 = "sha256-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX=";  # Replace with actual SHA256 from `nix-prefetch-url`
              };
            });
            grpc = super.grpc.overrideAttrs (old: rec {
              version = "1.54.0";
              src = super.fetchFromGitHub {
                owner = "grpc";
                repo = "grpc";
                rev = "v${version}";
                sha256 = "sha256-YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY=";  # Replace with actual SHA256
              };
            });
          })
        ];

        pkgs = import nixpkgs { inherit system; inherit overlays; };

        # Conditional for macOS-only tools.
        isDarwin = system == "aarch64-darwin" || system == "x86_64-darwin";
      in
      {
        # Formatter for consistent code style (e.g., `nix fmt`).
        formatter = pkgs.nixpkgs-fmt;

        # Apps for discoverability.
        apps.flake-info = {
          type = "app";
          program = "${pkgs.writeScriptBin "flake-info" ''
            echo "DCF Flake: Reproducible dev environments for SDKs in Perl, Python, C/C++, Node.js, Go, Rust, Android, iOS."
            echo "Usage: nix develop .#<sdk> (e.g., nix develop .#python) or nix develop for minimal shared tools."
            echo "Build: nix build .#c-sdk"
            echo "Tests: nix flake check"
          ''}/bin/flake-info";
        };

        devShells = {
          # Optimized default shell: Minimal shared tools only, using mkShellNoCC for lighter footprint.
          default = pkgs.mkShellNoCC {
            buildInputs = with pkgs; [ cmake git protobuf grpc which jq valgrind ];
            shellHook = ''
              export PATH=$PATH:${pkgs.protobuf}/bin:${pkgs.grpc}/bin
              echo "Minimal DCF Dev Environment Loaded (Shared Tools for Mono Repo SDKs)"
            '';
          };

          perl = pkgs.mkShell {
            buildInputs = with pkgs; [ perl perlPackages.JSON perlPackages.GrpcXs perlPackages.ModulePluggable perlPackages.CursesUI ];
            shellHook = "echo 'Perl SDK Dev Environment'";
          };

          python = pkgs.mkShell {
            buildInputs = with pkgs; [ python3 python3Packages.grpcio python3Packages.grpcio-tools python3Packages.importlib-metadata ];
            shellHook = "echo 'Python SDK Dev Environment'";
          };

          c = pkgs.mkShell {
            buildInputs = with pkgs; [ cmake protobuf protobufc libuuid cjson ncurses valgrind clang-format ];
            shellHook = ''
              export DCF_PLUGIN_PATH=${./c_sdk/plugins}
              echo 'C/C++ SDK Dev Environment (Plugin Path Set)'
            '';
          };

          nodejs = pkgs.mkShell {
            buildInputs = with pkgs; [ nodejs npm ];
            shellHook = "echo 'Node.js SDK Dev Environment'";
          };

          go = pkgs.mkShell {
            buildInputs = with pkgs; [ go gotools ];
            shellHook = "echo 'Go SDK Dev Environment'";
          };

          rust = pkgs.mkShell {
            buildInputs = with pkgs; [ rustc cargo rustfmt clippy tonic-build ];
            shellHook = "echo 'Rust SDK Dev Environment'";
          };

          android = pkgs.mkShell {
            buildInputs = with pkgs; [ androidenv.androidsdk_9_0 gradle jdk17 ];
            shellHook = ''
              export ANDROID_HOME=${pkgs.androidenv.androidsdk_9_0}/libexec/android-sdk
              export ANDROID_SDK_ROOT=$ANDROID_HOME
              echo 'Android SDK Dev Environment (Java/Kotlin) - Accept licenses if prompted'
            '';
          };

          # iOS shell only on macOS to avoid evaluation errors on Linux.
          ios = pkgs.lib.optionalAttrs isDarwin (pkgs.mkShell {
            buildInputs = with pkgs; [ swift xcodebuild ];
            shellHook = "echo 'iOS SDK Dev Environment (Swift) - macOS only'";
          });
        };

        packages = {
          # Native build for C SDK.
          c-sdk = pkgs.stdenv.mkDerivation {
            name = "dcf-c-sdk";
            src = ./c_sdk;
            buildInputs = with pkgs; [ cmake protobuf protobufc libuuid cjson ncurses ];
            buildPhase = "cmake . && make";
            installPhase = "make install DESTDIR=$out";
          };

          # Cross-compilation for aarch64-linux (e.g., Raspberry Pi).
          c-sdk-aarch64 = pkgs.pkgsCross.aarch64-multiplatform.stdenv.mkDerivation {
            name = "dcf-c-sdk-aarch64";
            src = ./c_sdk;
            buildInputs = with pkgs.pkgsCross.aarch64-multiplatform; [ cmake protobuf protobufc libuuid cjson ncurses ];
            buildPhase = "cmake . && make";
            installPhase = "make install DESTDIR=$out";
          };
        };

        # Checks for CI: Run tests for key SDKs.
        checks = {
          c-sdk-tests = pkgs.stdenv.mkDerivation {
            name = "dcf-c-sdk-tests";
            src = ./c_sdk;
            buildInputs = with pkgs; [ cmake protobuf protobufc libuuid cjson ncurses valgrind ];
            buildPhase = "cmake . && make test_redundancy test_plugin";
            checkPhase = "valgrind --leak-check=full ./p2p";
            installPhase = "mkdir -p $out";
          };

          python-tests = pkgs.stdenv.mkDerivation {
            name = "dcf-python-tests";
            src = ./python;
            buildInputs = with pkgs; [ python3 python3Packages.pytest python3Packages.grpcio ];
            checkPhase = "pytest tests/";
            installPhase = "mkdir -p $out";
          };
        };
      });
}
