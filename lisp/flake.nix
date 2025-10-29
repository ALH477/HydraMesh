{
  description = "Nix flake for HydraMesh (D-LISP) SDK";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=3d2d08824ce5f074a6ce9f74f9c26a8e5ba570e7"; # Pinned to a specific commit for determinism (nixpkgs 24.05 as of 2024-05-27; update as needed for 2025)
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true; # If any deps require it
        };

        # SBCL with required system packages (for CFFI, etc.)
        sbcl = pkgs.sbcl;

        # Quicklisp setup script for deterministic dist
        quicklisp-dist = "2023-10-21"; # Pinned Quicklisp dist version for determinism; update to a 2025 dist if available
        quicklisp-setup = pkgs.writeShellScriptBin "setup-quicklisp.sh" ''
          mkdir -p $HOME/quicklisp
          curl -O https://beta.quicklisp.org/quicklisp.lisp
          ${sbcl}/bin/sbcl --no-userinit --no-sysinit --load quicklisp.lisp \
            --eval '(quicklisp-quickstart:install :path "~/quicklisp/")' \
            --eval '(ql-util:without-prompting (ql:update-client) (ql:update-dist "quicklisp" :dist-version "${quicklisp-dist}"))' \
            --eval '(ql:quickload :ql-dist)' \
            --quit
        '';

        # List of Quicklisp packages from the code (for ql:quickload)
        ql-packages = [
          "cl-protobufs" "cl-grpc" "cffi" "uuid" "cl-json" "jsonschema"
          "cl-ppcre" "cl-csv" "usocket" "bordeaux-threads" "curses"
          "log4cl" "trivial-backtrace" "cl-store" "mgl" "hunchensocket"
          "fiveam" "cl-dot" "cl-lsquic" "cl-serial" "cl-can" "cl-sctp"
          "cl-zigbee" "cl-lorawan"
        ];

        # Script to load all Quicklisp deps deterministically
        load-deps = pkgs.writeShellScriptBin "load-deps.lisp" ''
          (load "~/quicklisp/setup.lisp")
          (ql:quickload '(${pkgs.lib.concatStringsSep " " (map (p: ":${p}") ql-packages)}))
          (quit)
        '';

        # Build the D-LISP executable using sbcl --load
        hydramesh = pkgs.stdenv.mkDerivation {
          name = "hydramesh";
          src = self; # Use the flake source (assuming hydramesh.lisp is in the repo root)

          nativeBuildInputs = [ sbcl quicklisp-setup ];

          buildPhase = ''
            export HOME=$PWD
            setup-quicklisp.sh

            # Load dependencies
            ${sbcl}/bin/sbcl --script ${load-deps}/bin/load-deps.lisp

            # Build the executable as per dcf-deploy
            ${sbcl}/bin/sbcl --no-userinit --load hydramesh.lisp \
              --eval '(dcf-deploy "dcf-lisp")' \
              --quit
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp dcf-lisp $out/bin/dcf-lisp
          '';

          # Note: libstreamdb.so/.wasm needs to be provided separately; this assumes it's in LD_LIBRARY_PATH or built elsewhere
          # For full determinism, add derivations for libstreamdb if source is available
        };

      in {
        packages.default = hydramesh;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            sbcl
            quicklisp-setup
            # Add system deps for CFFI bindings (e.g., for gRPC, etc.)
            pkgs.grpc
            pkgs.protobuf
            pkgs.openssl
            pkgs.zlib
          ];

          shellHook = ''
            export HOME=$PWD
            if [ ! -d "$HOME/quicklisp" ]; then
              setup-quicklisp.sh
            fi
            echo "Quicklisp set up with dist ${quicklisp-dist}. Load deps with: sbcl --script ${load-deps}/bin/load-deps.lisp"
            echo "Load the project: sbcl --load hydramesh.lisp"
          '';
        };
      }
    );
}
