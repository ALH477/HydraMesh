{
  description = "DeMoD Communication Framework (DCF) Mono Repo SDKs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        protobuf = pkgs.protobuf;
        # Shared proto generation function
        generateProtos = lang: outDir: flags: pkgs.runCommand "dcf-protos-${lang}" {} ''
          mkdir -p $out/${outDir}
          ${protobuf}/bin/protoc -I${self} ${flags} ${self}/messages.proto ${self}/services.proto
          cp -r . $out/${outDir}
        '';
      in
      {
        packages = rec {

          # C SDK
          dcf-c = pkgs.stdenv.mkDerivation {
            pname = "dcf-c";
            version = "0.1.0";
            src = self + "/c_sdk";
            nativeBuildInputs = [ pkgs.cmake pkgs.pkg-config ];
            buildInputs = [ pkgs.libprotobufc pkgs.libuuid pkgs.cjson pkgs.ncurses ];
            configurePhase = "cmakeConfigurePhase";
            buildPhase = "make";
            installPhase = "make install DESTDIR=$out";
            meta.description = "C SDK for DCF";
          };

          # C++ SDK (assumes proto gen in build)
          dcf-cpp = pkgs.stdenv.mkDerivation {
            pname = "dcf-cpp";
            version = "0.1.0";
            src = self + "/cpp";
            nativeBuildInputs = [ protobuf pkgs.grpc pkgs.cmake ];
            preBuild = ''
              ${protobuf}/bin/protoc -I${self} --cpp_out=src --grpc_out=src --plugin=protoc-gen-grpc=${pkgs.grpc}/bin/grpc_cpp_plugin ${self}/messages.proto ${self}/services.proto
            '';
            buildPhase = "cmake . && make";
            installPhase = "make install DESTDIR=$out";
            meta.description = "C++ SDK for DCF";
          };

          # Go SDK
          dcf-go = pkgs.buildGoModule {
            pname = "dcf-go";
            version = "0.1.0";
            src = self + "/go";
            vendorHash = "sha256-0000000000000000000000000000000000000000000="; # Update with actual
            preBuild = ''
              ${protobuf}/bin/protoc -I${self} --go_out=. --go-grpc_out=. ${self}/messages.proto ${self}/services.proto
            '';
            meta.description = "Go SDK for DCF";
          };

          # Lisp SDK (dev shell oriented, as it's interpreted)
          # For package, perhaps build StreamDB and provide loader
          streamdb = pkgs.rustPlatform.buildRustPackage {
            pname = "streamdb";
            version = "0.1.0";
            src = self + "/streamdb";
            cargoHash = "sha256-0000000000000000000000000000000000000000000="; # Update with actual
            buildType = "release";
            meta.description = "StreamDB for DCF Lisp SDK";
          };

          # Python SDK
          dcf-python = pkgs.python3Packages.buildPythonPackage {
            pname = "dcf-python";
            version = "0.1.0";
            src = self + "/python";
            propagatedBuildInputs = with pkgs.python3Packages; [ protobuf grpcio grpcio-tools ];
            preBuild = ''
              python -m grpc_tools.protoc -I${self} --python_out=dcf --grpc_python_out=dcf ${self}/messages.proto ${self}/services.proto
            '';
            meta.description = "Python SDK for DCF";
          };

          # Rust SDK
          dcf-rust = pkgs.rustPlatform.buildRustPackage {
            pname = "dcf-rust";
            version = "0.1.0";
            src = self + "/rust";
            cargoHash = "sha256-0000000000000000000000000000000000000000000="; # Update with actual
            nativeBuildInputs = [ protobuf ];
            buildPhase = ''
              ${pkgs.tonic-build}/bin/tonic-build --build-server --out-dir src ${self}/services.proto
              cargo build --release
            '';
            meta.description = "Rust SDK for DCF";
          };

          # Node.js SDK
          dcf-nodejs = pkgs.nodePackages.buildNpmPackage {
            pname = "dcf-nodejs";
            version = "0.1.0";
            src = self + "/nodejs";
            npmDepsHash = "sha256-0000000000000000000000000000000000000000000="; # Update with actual
            nativeBuildInputs = [ protobuf pkgs.grpc-tools.grpc_tools_node_protoc_ts ];
            preBuild = ''
              protoc -I${self} --js_out=import_style=commonjs,binary:src --grpc_out=grpc_js:src --plugin=protoc-gen-grpc=${pkgs.grpc-tools}/bin/grpc_node_plugin ${self}/messages.proto ${self}/services.proto
            '';
            meta.description = "Node.js SDK for DCF";
          };

          # Perl SDK (wrapper for modules)
          dcf-perl = pkgs.stdenv.mkDerivation {
            pname = "dcf-perl";
            version = "0.1.0";
            src = self + "/perl";
            nativeBuildInputs = [ protobuf pkgs.perl pkgs.perlPackages.GrpcXs ];
            buildInputs = with pkgs.perlPackages; [ JSON IOSocketINET GetoptLong CursesUI GoogleProtocolBuffersDynamic ModulePluggable ];
            preBuild = ''
              ${protobuf}/bin/protoc -I${self} --perl_out=lib ${self}/messages.proto ${self}/services.proto
            '';
            installPhase = "cp -r lib $out/lib";
            meta.description = "Perl SDK for DCF";
          };

          # Docs
          dcf-docs = pkgs.stdenv.mkDerivation {
            pname = "dcf-docs";
            version = "0.1.0";
            src = self + "/docs";
            nativeBuildInputs = pkgs.python3.withPackages (ps: with ps; [ sphinx myst-parser ]);
            buildPhase = "make html";
            installPhase = "cp -r _build/html $out";
            meta.description = "DCF Documentation";
          };

          # Default package (e.g., all SDKs)
          default = dcf-c; # Or a collection
        };

        devShells = {
          default = pkgs.mkShell {
            packages = [ protobuf pkgs.cmake pkgs.grpc pkgs.rustc pkgs.cargo pkgs.go pkgs.nodejs pkgs.python3 pkgs.perl pkgs.sbcl ];
            shellHook = ''
              export PROTOC=${protobuf}/bin/protoc
            '';
            meta.description = "Dev shell for DCF mono repo";
          };

          lisp = pkgs.mkShell {
            packages = [ pkgs.sbcl pkgs.quicklisp ];
            inputsFrom = [ self.packages.${system}.streamdb ];
            shellHook = ''
              export LD_LIBRARY_PATH=${self.packages.${system}.streamdb}/lib
              sbcl --load ${self}/lisp/src/d-lisp.lisp
            '';
            meta.description = "Lisp SDK dev shell";
          };
        };
      }
    );
}
