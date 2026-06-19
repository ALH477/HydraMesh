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

          # C SDK — builds the dcfnode mesh node (ProtoMessage/UDP + Faust modem).
          # src is the whole repo because the node includes the header-only codec
          # at ../codec; the build only targets dcfnode (the 4-module spine is
          # configured but not linked here).
          dcf-c = pkgs.stdenv.mkDerivation {
            pname = "dcf-c";
            version = "0.3.0";
            src = self;
            nativeBuildInputs = [ pkgs.cmake ];
            dontUseCmakeConfigure = true;
            buildPhase = ''
              runHook preBuild
              cmake -S C_SDK -B build \
                -DDCF_BUILD_NODE=ON -DDCF_BUILD_TESTS=OFF -DDCF_BUILD_EXAMPLES=OFF
              cmake --build build --target dcfnode -j''${NIX_BUILD_CORES:-2}
              runHook postBuild
            '';
            installPhase = ''
              mkdir -p $out/bin
              cp build/dcfnode $out/bin/
            '';
            meta.description = "DCF C SDK node (dcfnode: ProtoMessage/UDP + Faust modem)";
            meta.license = pkgs.lib.licenses.lgpl3Only;
            meta.mainProgram = "dcfnode";
          };

          # C++ SDK — the supercharged gRPC node (dcfcpp). CMake's find_package
          # picks up gRPC/protobuf and runs the C++/grpc codegen on cpp/proto/dcf.proto;
          # the Nix derivation provides the full transitive closure (re2/absl/...).
          dcf-cpp = pkgs.stdenv.mkDerivation {
            pname = "dcf-cpp";
            version = "0.3.0";
            src = self + "/cpp";
            nativeBuildInputs = [ pkgs.cmake pkgs.pkg-config pkgs.protobuf pkgs.grpc ];
            buildInputs = [ pkgs.grpc pkgs.protobuf pkgs.openssl pkgs.zlib pkgs.c-ares pkgs.re2 pkgs.abseil-cpp ];
            cmakeFlags = [ "-DDCF_CPP_GRPC=ON" ];
            # Build only the node target (the certify tests need ../Documentation at run
            # time and are exercised separately).
            buildPhase = "cmake --build . --target dcfcpp -j$NIX_BUILD_CORES";
            installPhase = ''
              mkdir -p $out/bin
              cp dcfcpp $out/bin/
            '';
            meta.description = "DCF C++ gRPC node (dcfcpp)";
            meta.license = pkgs.lib.licenses.lgpl3Only;
            meta.mainProgram = "dcfcpp";
          };

          # Go SDK — builds the dcfnode CLI (a real UDP DeModFrame node, the Go
          # analogue of the Rust `dcf` binary). The module is stdlib-only (no
          # external deps, no gRPC/proto), so vendorHash = null.
          dcf-go = pkgs.buildGoModule {
            pname = "dcf-go";
            version = "0.3.0";
            src = self + "/go";
            vendorHash = null; # stdlib-only: nothing to vendor
            subPackages = [ "cmd/dcfnode" ];
            meta.description = "DCF Go SDK node (dcfnode CLI)";
            meta.license = pkgs.lib.licenses.lgpl3Only;
            meta.mainProgram = "dcfnode";
          };

          # Lisp SDK (dev shell oriented, as it's interpreted)
          # For package, perhaps build StreamDB and provide loader
          streamdb = pkgs.rustPlatform.buildRustPackage {
            pname = "streamdb";
            version = "0.3.0";
            src = self + "/lisp/streamdb";
            cargoHash = pkgs.lib.fakeHash; # TODO: real hash via `nix build` (needs a nix environment)
            buildType = "release";
            meta.description = "StreamDB for DCF Lisp SDK";
            meta.license = pkgs.lib.licenses.lgpl3Only;
          };

          # Python SDK
          dcf-python = pkgs.python3Packages.buildPythonPackage {
            pname = "dcf-python";
            version = "0.3.0";
            src = self + "/python";
            propagatedBuildInputs = with pkgs.python3Packages; [ protobuf grpcio grpcio-tools ];
            preBuild = ''
              python -m grpc_tools.protoc -I${self} --python_out=dcf --grpc_python_out=dcf ${self}/messages.proto ${self}/services.proto
            '';
            meta.description = "Python SDK for DCF";
            meta.license = pkgs.lib.licenses.lgpl3Only;
          };

          # Rust SDK — the `dcf` node binary. Built from the whole repo so the
          # crate's path dependency on ../codec resolves; build.rs runs tonic-build
          # itself (it just needs protoc via PROTOC), so no manual codegen phase.
          dcf-rust = pkgs.rustPlatform.buildRustPackage {
            pname = "dcf-rust";
            version = "0.3.0";
            src = self;
            cargoRoot = "rust";
            buildAndTestSubdir = "rust";
            cargoLock.lockFile = self + "/rust/Cargo.lock"; # deterministic — local path dep, no hash
            nativeBuildInputs = [ protobuf ];
            PROTOC = "${protobuf}/bin/protoc";
            doCheck = false;
            meta.description = "DCF Rust SDK node (dcf binary)";
            meta.license = pkgs.lib.licenses.lgpl3Only;
            meta.mainProgram = "dcf";
          };

          # FFmpeg with the native DCF-Audio demuxer built in. DCF-Audio is an
          # adapter over the 17-byte wire quantum; ff_dcf_demuxer reuses the
          # certified C reassembler (codec/demod_audio.h) so `ffmpeg -f dcf -i
          # capture.dcf out.flac` records audio off the wire into any file type.
          # See ffmpeg-dcf/README.md. The PM synth (codec_id 2) is compiled in
          # from codec/faust/dcf_pm_codec.gen.c; codec_id 0/1 ride built-in
          # opus / pcm_u8 decoders, so only a demuxer is registered.
          dcf-ffmpeg = pkgs.ffmpeg.overrideAttrs (old: {
            pname = "dcf-ffmpeg";
            postPatch = (old.postPatch or "") + ''
              cp ${self}/ffmpeg-dcf/dcfdec.c            libavformat/dcfdec.c
              cp ${self}/ffmpeg-dcf/dcf_pm_ff.c         libavformat/dcf_pm_ff.c
              cp ${self}/codec/demod_frame.h            libavformat/demod_frame.h
              cp ${self}/codec/demod_audio.h            libavformat/demod_audio.h
              cp ${self}/codec/faust/dcf_pm_codec.gen.c libavformat/dcf_pm_codec.gen.c
              cp ${self}/codec/faust/dcf_pm_faust.c     libavformat/dcf_pm_faust.c
              # Register the demuxer (configure's find_things_extern scans this file).
              sed -i '/extern const FFInputFormat  *ff_aa_demuxer;/a extern const FFInputFormat  ff_dcf_demuxer;' \
                  libavformat/allformats.c
              # Link the demuxer + PM synth (compiled via the missing-prototypes
              # build shim) when CONFIG_DCF_DEMUXER is on.
              printf '\nOBJS-$(CONFIG_DCF_DEMUXER) += dcfdec.o dcf_pm_ff.o\n' \
                  >> libavformat/Makefile
            '';
            configureFlags = (old.configureFlags or [ ]) ++ [ "--enable-demuxer=dcf" ];
            doCheck = false;
            meta = (old.meta or { }) // {
              description = "FFmpeg with the native DCF-Audio (dcf) demuxer";
            };
          });

          # CLI front door to the `dcf` demuxer: record / inspect / split / listen.
          # Python (reuses the certified python/MCP + python/dcf modules) and shells
          # ffmpeg/ffprobe from dcf-ffmpeg. See ffmpeg-dcf/dcf-rec + README.
          dcf-rec = pkgs.writeShellApplication {
            name = "dcf-rec";
            runtimeInputs = [ dcf-ffmpeg pkgs.python3 ];
            text = ''exec python3 ${self}/ffmpeg-dcf/dcf-rec "$@"'';
          };

          # ── OCI node images (hermetic; built with `nix build .#docker-*`) ──────
          # These run like real nodes, not cert harnesses: each image's entrypoint
          # is the node binary with `start` as the default command.
          docker-dcf-go = pkgs.dockerTools.buildLayeredImage {
            name = "alh477/dcf-go";
            tag = "latest";
            contents = [ dcf-go pkgs.cacert ];
            config = {
              Entrypoint = [ "${dcf-go}/bin/dcfnode" ];
              Cmd = [ "start" ];
              ExposedPorts = { "7777/udp" = { }; };
              Labels = { "org.opencontainers.image.source" = "https://github.com/ALH477/HydraMesh"; };
            };
          };

          docker-dcf-rust = pkgs.dockerTools.buildLayeredImage {
            name = "alh477/dcf-rs";
            tag = "latest";
            contents = [ dcf-rust pkgs.cacert ];
            config = {
              Entrypoint = [ "${dcf-rust}/bin/dcf" ];
              Cmd = [ "start" ];
              ExposedPorts = { "7777/udp" = { }; "50051/tcp" = { }; };
              Labels = { "org.opencontainers.image.source" = "https://github.com/ALH477/HydraMesh"; };
            };
          };

          # Python node: the stdlib mesh endpoint (matrix-bridge/a2a.py over
          # DcfTextNode — bare DeModFrame + SuperPack dialect). `recv --follow`
          # is a persistent listener; `send "msg" --peers h:p` unicasts.
          dcf-python-node = pkgs.writeShellApplication {
            name = "dcf-node";
            runtimeInputs = [ pkgs.python3 ];
            text = ''exec python3 ${self}/matrix-bridge/a2a.py "$@"'';
          };
          docker-dcf-python = pkgs.dockerTools.buildLayeredImage {
            name = "alh477/dcf-python";
            tag = "latest";
            contents = [ dcf-python-node ];
            config = {
              Entrypoint = [ "${dcf-python-node}/bin/dcf-node" ];
              Cmd = [ "recv" "--follow" ];
              ExposedPorts = { "7801/udp" = { }; };
              Labels = { "org.opencontainers.image.source" = "https://github.com/ALH477/HydraMesh"; };
            };
          };

          # C node image: dcfnode (ProtoMessage/UDP, meshes with Go/Rust; plus the
          # Faust modem send-modem/recv-modem over a file medium).
          docker-dcf-c = pkgs.dockerTools.buildLayeredImage {
            name = "alh477/dcf-c";
            tag = "latest";
            contents = [ dcf-c pkgs.cacert ];
            config = {
              Entrypoint = [ "${dcf-c}/bin/dcfnode" ];
              Cmd = [ "start" ];
              ExposedPorts = { "7777/udp" = { }; };
              Labels = { "org.opencontainers.image.source" = "https://github.com/ALH477/HydraMesh"; };
            };
          };

          # C++ node image: the supercharged gRPC node (dcfcpp).
          docker-dcf-cpp = pkgs.dockerTools.buildLayeredImage {
            name = "alh477/dcf-cpp";
            tag = "latest";
            contents = [ dcf-cpp pkgs.cacert ];
            config = {
              Entrypoint = [ "${dcf-cpp}/bin/dcfcpp" ];
              Cmd = [ "serve" ];
              ExposedPorts = { "50051/tcp" = { }; };
              Labels = { "org.opencontainers.image.source" = "https://github.com/ALH477/HydraMesh"; };
            };
          };

          # Node.js node: a stdlib `dgram` UDP node (JS/nodejs/src/node.js) in the
          # same bare-DeModFrame + SuperPack dialect as the Python node — they mesh.
          dcf-nodejs-node = pkgs.writeShellApplication {
            name = "dcf-node-js";
            runtimeInputs = [ pkgs.nodejs ];
            text = ''exec node ${self}/JS/nodejs/src/node.js "$@"'';
          };
          docker-dcf-nodejs = pkgs.dockerTools.buildLayeredImage {
            name = "alh477/dcf-nodejs";
            tag = "latest";
            contents = [ dcf-nodejs-node ];
            config = {
              Entrypoint = [ "${dcf-nodejs-node}/bin/dcf-node-js" ];
              Cmd = [ "recv" "--follow" ];
              ExposedPorts = { "7801/udp" = { }; };
              Labels = { "org.opencontainers.image.source" = "https://github.com/ALH477/HydraMesh"; };
            };
          };

          # Node.js SDK
          dcf-nodejs = pkgs.nodePackages.buildNpmPackage {
            pname = "dcf-nodejs";
            version = "0.3.0";
            src = self + "/JS/nodejs";
            npmDepsHash = pkgs.lib.fakeHash; # TODO: real hash via `nix build` (needs a nix environment)
            nativeBuildInputs = [ protobuf pkgs.grpc-tools.grpc_tools_node_protoc_ts ];
            preBuild = ''
              protoc -I${self} --js_out=import_style=commonjs,binary:src --grpc_out=grpc_js:src --plugin=protoc-gen-grpc=${pkgs.grpc-tools}/bin/grpc_node_plugin ${self}/messages.proto ${self}/services.proto
            '';
            meta.description = "Node.js SDK for DCF";
            meta.license = pkgs.lib.licenses.lgpl3Only;
          };

          # Perl SDK (wrapper for modules)
          dcf-perl = pkgs.stdenv.mkDerivation {
            pname = "dcf-perl";
            version = "0.3.0";
            src = self + "/perl";
            nativeBuildInputs = [ protobuf pkgs.perl pkgs.perlPackages.GrpcXs ];
            buildInputs = with pkgs.perlPackages; [ JSON IOSocketINET GetoptLong CursesUI GoogleProtocolBuffersDynamic ModulePluggable ];
            preBuild = ''
              ${protobuf}/bin/protoc -I${self} --perl_out=lib ${self}/messages.proto ${self}/services.proto
            '';
            installPhase = "cp -r lib $out/lib";
            meta.description = "Perl SDK for DCF";
            meta.license = pkgs.lib.licenses.lgpl3Only;
          };

          # Docs
          dcf-docs = pkgs.stdenv.mkDerivation {
            pname = "dcf-docs";
            version = "0.3.0";
            src = self + "/Documentation";
            nativeBuildInputs = pkgs.python3.withPackages (ps: with ps; [ sphinx myst-parser ]);
            buildPhase = "make html";
            installPhase = "cp -r _build/html $out";
            meta.description = "DCF Documentation";
            meta.license = pkgs.lib.licenses.lgpl3Only;
          };

          # Default package (e.g., all SDKs)
          default = dcf-c; # Or a collection
        };

        # Runnable entry points for the agent-to-agent mesh feature.
        #   nix run github:ALH477/HydraMesh#a2a         guided setup + run
        #   nix run github:ALH477/HydraMesh#a2a-demo    stdlib loopback smoke test
        #   nix run github:ALH477/HydraMesh#a2a-config  generate an MCP config for any agent client
        #   nix run github:ALH477/HydraMesh#a2a-send    send a message onto the mesh (no MCP)
        #   nix run github:ALH477/HydraMesh#a2a-recv    receive mesh text (no MCP)
        #   nix run github:ALH477/HydraMesh#mesh-agent  the DeModFrame MCP endpoint (stdio)
        #   nix run github:ALH477/HydraMesh#mesh-http   shared HTTP mesh service (mesh_mcp.py http)
        #   nix run github:ALH477/HydraMesh#mesh-viz    live web dashboard of mesh agents
        apps =
          let
            # The matrix-bridge/*.py scripts self-locate their imports (they add
            # matrix-bridge/ and python/MCP/ to sys.path), so they only need a
            # Python interpreter on PATH — no install/build step.
            mkA2A = name: tail: pkgs.writeShellApplication {
              inherit name;
              runtimeInputs = [ pkgs.python3 ];
              text = ''exec python3 ${self}/matrix-bridge/${tail} "$@"'';
            };
            interactive = mkA2A "dcf-a2a" "a2a_interactive.py";
            demo = mkA2A "dcf-a2a-demo" "a2a_runner.py --demo";
            viz = mkA2A "dcf-mesh-viz" "mesh_viz.py";   # stdlib web dashboard
            # mesh_mcp.py is the MCP server an agent connects to; it needs `mcp`.
            meshPython = pkgs.python3.withPackages (ps: [ ps.mcp ]);
            meshAgent = pkgs.writeShellApplication {
              name = "dcf-mesh-agent";
              runtimeInputs = [ meshPython ];
              text = ''exec ${meshPython}/bin/python ${self}/matrix-bridge/mesh_mcp.py "$@"'';
            };
            # Multi-agent client integration: the a2a.py umbrella (config/send/recv are
            # stdlib) and a shared HTTP mesh service (mesh_mcp.py http, needs `mcp`).
            a2aConfig = mkA2A "dcf-a2a-config" "a2a.py config";
            a2aSend = mkA2A "dcf-a2a-send" "a2a.py send";
            a2aRecv = mkA2A "dcf-a2a-recv" "a2a.py recv";
            meshHttp = pkgs.writeShellApplication {
              name = "dcf-mesh-http";
              runtimeInputs = [ meshPython ];
              text = ''exec ${meshPython}/bin/python ${self}/matrix-bridge/mesh_mcp.py http "$@"'';
            };
            mkApp = drv: bin: { type = "app"; program = "${drv}/bin/${bin}"; };
          in {
            a2a = mkApp interactive "dcf-a2a";
            a2a-demo = mkApp demo "dcf-a2a-demo";
            a2a-config = mkApp a2aConfig "dcf-a2a-config";
            a2a-send = mkApp a2aSend "dcf-a2a-send";
            a2a-recv = mkApp a2aRecv "dcf-a2a-recv";
            mesh-agent = mkApp meshAgent "dcf-mesh-agent";
            mesh-http = mkApp meshHttp "dcf-mesh-http";
            mesh-viz = mkApp viz "dcf-mesh-viz";
            default = mkApp interactive "dcf-a2a";
          };

        devShells = {
          default = pkgs.mkShell {
            packages = [
              protobuf pkgs.cmake pkgs.grpc pkgs.rustc pkgs.cargo pkgs.go
              pkgs.nodejs pkgs.python3 pkgs.perl pkgs.sbcl
              # DCF-Audio -> ffmpeg recording: the `dcf` demuxer + dcf-rec wrapper.
              self.packages.${system}.dcf-ffmpeg self.packages.${system}.dcf-rec
            ];
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

          # HydraMesh communications client (Tauri 2: client/). Build/run with:
          #   nix develop .#comms
          #   cd client && npm install && cargo tauri dev
          comms = pkgs.mkShell {
            nativeBuildInputs = [
              pkgs.pkg-config pkgs.rustc pkgs.cargo pkgs.nodejs pkgs.cargo-tauri
            ];
            buildInputs = [
              # Tauri webview (Linux)
              pkgs.glib pkgs.gtk3 pkgs.webkitgtk_4_1 pkgs.libsoup_3
              pkgs.gdk-pixbuf pkgs.cairo pkgs.pango pkgs.atk pkgs.openssl
              pkgs.dbus pkgs.librsvg
              # Real-time audio + Opus (cpal + the `audio` feature)
              pkgs.alsa-lib pkgs.libopus
            ];
            shellHook = ''
              echo "◈ HydraMesh comms client — cd client && npm install && cargo tauri dev"
            '';
            meta.description = "HydraMesh Tauri comms client dev shell";
          };
        };
      }
    );
}
