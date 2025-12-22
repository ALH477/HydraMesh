{
  description = "Nix flake for HydraMesh (D-LISP) SDK – Emacs + SLY focused development";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        };

        # List of required Quicklisp systems from the original code
        qlSystems = [
          "cffi" "uuid" "cl-protobufs" "usocket" "bordeaux-threads"
          "log4cl" "trivial-backtrace" "flexi-streams" "fiveam"
          "ieee-floats" "cl-json" "jsonschema"
        ];

        # SBCL with all required dependencies pre-loaded via Nixpkgs lispPackages
        sbclWithDeps = pkgs.sbcl.withPackages (ps: with ps; [
          cffi uuid cl-protobufs usocket bordeauxThreads
          log4cl trivial-backtrace flexiStreams fiveam
          ieee-floats cl-json jsonschema
        ]);

        # Emacs with native compilation and SLY
        emacsWithSly = pkgs.emacsNativeComp.pkgs.withPackages (epkgs: [
          epkgs.sly
        ]);

        # The HydraMesh executable – robust production build
        hydramesh = pkgs.stdenv.mkDerivation {
          pname = "hydramesh";
          version = "2.2.0";

          src = self;

          nativeBuildInputs = [ sbclWithDeps ];

          # Critical: prevent stripping which breaks SBCL executables
          dontStrip = true;

          buildPhase = ''
            ${sbclWithDeps}/bin/sbcl --no-userinit --non-interactive \
              --load hydramesh.lisp \
              --eval '(dcf-deploy "hydramesh")' \
              --quit
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp hydramesh $out/bin/hydramesh
          '';

          meta = with pkgs.lib; {
            description = "HydraMesh SDK executable";
            license = licenses.lgpl3;
            platforms = platforms.all;
          };
        };

      in {
        packages.default = hydramesh;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            sbclWithDeps
            emacsWithSly
            pkgs.grpc
            pkgs.protobuf
            pkgs.openssl
            pkgs.zlib
            # Optional: for easier dependency management during dev
            pkgs.roswell
          ];

          shellHook = ''
            # Create minimal Emacs init with SLY configured for SBCL
            mkdir -p $HOME/.emacs.d
            cat > $HOME/.emacs.d/init.el <<'EOF'
            ;; Minimal SLY configuration for HydraMesh development
            (require 'sly)
            (setq inferior-lisp-program "${sbclWithDeps}/bin/sbcl")
            (add-hook 'lisp-mode-hook #'sly-mode)
            (add-hook 'slime-repl-mode-hook #'sly-mrepl-mode)
            ;; Optional: enable company completion
            (add-hook 'sly-mode-hook #'company-mode)
            EOF

            echo "══════════════════════════════════════════════════════════════"
            echo "HydraMesh development shell (Emacs + SLY) ready!"
            echo "• Start Emacs: emacs"
            echo "• In Emacs: M-x sly  → connects to SBCL with all deps loaded"
            echo "• Open hydramesh.lisp and evaluate forms with C-x C-e"
            echo "• Build executable: nix build .#"
            echo "• Run tests: (fiveam:run! 'hydramesh-suite) in REPL"
            echo "══════════════════════════════════════════════════════════════"
          '';
        };
      }
    );
}
