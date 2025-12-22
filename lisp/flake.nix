{
  description = "Nix flake for HydraMesh (D-LISP) SDK";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";  # For latest Emacs + packages like sly
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
          config.allowUnfree = true;
        };

        # Emacs with native compilation and SLY
        emacsWithPackages = (pkgs.emacsWithPackagesFromUsePackage {
          package = pkgs.emacsNativeComp;  # Or pkgs.emacsGtk for GTK build
          config = ./emacs-config.el;  # Optional: put your full init.el here
          defaultInitFile = true;

          alwaysEnsure = true;

          extraEmacsPackages = epkgs: [
            epkgs.sly
            # Add more if needed, e.g., epkgs.use-package, epkgs.magit, etc.
          ];
        });

        # The HydraMesh executable (no Quicklisp preloading)
        hydramesh = pkgs.stdenv.mkDerivation {
          name = "hydramesh";
          src = self;

          nativeBuildInputs = [ pkgs.sbcl ];

          buildPhase = ''
            ${pkgs.sbcl}/bin/sbcl --no-userinit --load hydramesh.lisp \
              --eval '(dcf-deploy "dcf-lisp")' \
              --quit
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp dcf-lisp $out/bin/dcf-lisp
          '';

          # Note: libstreamdb.so/.dylib/.wasm must be available in runtime path
        };

      in {
        packages.default = hydramesh;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.sbcl
            emacsWithPackages
            pkgs.grpc
            pkgs.protobuf
            pkgs.openssl
            pkgs.zlib
            # Add other system deps for CFFI if needed
          ];

          shellHook = ''
            echo "HydraMesh devShell ready!"
            echo " - Run 'emacs' to start Emacs with SLY configured."
            echo " - In Emacs: M-x sly to connect to SBCL REPL (inferior-lisp-program is set to sbcl)."
            echo " - Load your project: C-x C-f hydramesh.lisp, then M-x sly"
            echo "Basic SLY config applied via environment (expand in ~/.emacs.d/init.el if needed)."
          '';

          # Minimal SLY setup injected into Emacs environment
          EMACSLOADPATH = "${emacsWithPackages.deps}/share/emacs/site-lisp/elpa/*:";
        };
      }
    );
}
