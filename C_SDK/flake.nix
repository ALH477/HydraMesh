{
  description = "DCF - DeMoD Communications Framework";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Common build inputs
        buildInputs = with pkgs; [
          cmake
          gnumake
          pkg-config
        ];
        
        # Development dependencies
        devInputs = with pkgs; [
          gdb
          valgrind
          clang-tools
          cppcheck
          include-what-you-use
          lcov
          gcovr
          doxygen
          graphviz
        ];
        
        # The main DCF package
        # NOTE: Shared library disabled due to TLS relocation issues with NixOS toolchain.
        # The GCC in nixpkgs generates initial-exec TLS relocations even with
        # -ftls-model=global-dynamic, which are incompatible with shared objects.
        # Use dcf-shared if you need the .so (requires manual LD_PRELOAD workarounds).
        dcf = pkgs.stdenv.mkDerivation rec {
          pname = "dcf";
          version = "5.2.0";
          
          src = ./.;
          
          nativeBuildInputs = buildInputs;
          
          cmakeFlags = [
            "-DCMAKE_BUILD_TYPE=Release"
            "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
            "-DDCF_BUILD_SHARED=OFF"
            "-DDCF_BUILD_STATIC=ON"
            "-DDCF_BUILD_TESTS=ON"
            "-DDCF_BUILD_EXAMPLES=ON"
            "-DDCF_ENABLE_LTO=ON"
          ];
          
          doCheck = true;
          checkPhase = ''
            ctest --output-on-failure
          '';
          
          # Skip ldconfig on NixOS (read-only /nix/store)
          postInstall = ''
            rm -f $out/lib/*.la 2>/dev/null || true
          '';
          
          meta = with pkgs.lib; {
            description = "DeMoD Communications Framework";
            homepage = "https://github.com/example/dcf";
            license = licenses.mit;
            platforms = platforms.unix;
            maintainers = [];
          };
        };
        
        # Debug build variant
        dcf-debug = dcf.overrideAttrs (oldAttrs: {
          pname = "dcf-debug";
          cmakeFlags = [
            "-DCMAKE_BUILD_TYPE=Debug"
            "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
            "-DDCF_BUILD_SHARED=OFF"
            "-DDCF_BUILD_STATIC=ON"
            "-DDCF_BUILD_TESTS=ON"
            "-DDCF_BUILD_EXAMPLES=ON"
            "-DDCF_ENABLE_LTO=OFF"
          ];
          dontStrip = true;
        });
        
        # Shared library build - requires clang which handles TLS better
        dcf-shared = pkgs.stdenv.mkDerivation {
          pname = "dcf-shared";
          version = "5.2.0";
          
          src = ./.;
          
          nativeBuildInputs = buildInputs ++ [ pkgs.llvmPackages.clang ];
          
          cmakeFlags = [
            "-DCMAKE_BUILD_TYPE=Release"
            "-DCMAKE_C_COMPILER=clang"
            "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
            "-DDCF_BUILD_SHARED=ON"
            "-DDCF_BUILD_STATIC=ON"
            "-DDCF_BUILD_TESTS=ON"
            "-DDCF_BUILD_EXAMPLES=ON"
            "-DDCF_ENABLE_LTO=OFF"
          ];
          
          # Clang respects TLS model flags properly
          NIX_CFLAGS_COMPILE = "-ftls-model=global-dynamic";
          
          doCheck = true;
          checkPhase = ''
            ctest --output-on-failure
          '';
          
          postInstall = ''
            rm -f $out/lib/*.la 2>/dev/null || true
          '';
          
          meta = with pkgs.lib; {
            description = "DCF with shared library (built with clang)";
            license = licenses.mit;
            platforms = platforms.unix;
          };
        };
        
        # Sanitizer build (for testing)
        dcf-sanitized = dcf.overrideAttrs (oldAttrs: {
          pname = "dcf-sanitized";
          
          nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.llvmPackages.clang ];
          
          cmakeFlags = [
            "-DCMAKE_BUILD_TYPE=Debug"
            "-DCMAKE_C_COMPILER=clang"
            "-DDCF_BUILD_SHARED=OFF"
            "-DDCF_BUILD_STATIC=ON"
            "-DDCF_BUILD_TESTS=ON"
            "-DDCF_ENABLE_SANITIZERS=ON"
          ];
          
          # Sanitizers need special runtime flags
          ASAN_OPTIONS = "detect_leaks=1:abort_on_error=1";
          UBSAN_OPTIONS = "print_stacktrace=1:halt_on_error=1";
          
          dontStrip = true;
        });
        
        # Coverage build
        dcf-coverage = dcf.overrideAttrs (oldAttrs: {
          pname = "dcf-coverage";
          
          nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.lcov pkgs.gcovr ];
          
          cmakeFlags = [
            "-DCMAKE_BUILD_TYPE=Debug"
            "-DDCF_BUILD_SHARED=OFF"
            "-DDCF_BUILD_STATIC=ON"
            "-DDCF_BUILD_TESTS=ON"
            "-DDCF_ENABLE_COVERAGE=ON"
          ];
          
          postCheck = ''
            # Generate coverage report
            lcov --capture --directory . --output-file coverage.info
            lcov --remove coverage.info '/nix/*' --output-file coverage.info
            genhtml coverage.info --output-directory coverage-report
            mkdir -p $out/share/coverage
            cp -r coverage-report $out/share/coverage/
          '';
          
          dontStrip = true;
        });

      in {
        # Packages
        packages = {
          default = dcf;
          inherit dcf dcf-debug dcf-shared dcf-sanitized dcf-coverage;
        };
        
        # Development shells
        devShells = {
          default = pkgs.mkShell {
            name = "dcf-dev";
            
            packages = buildInputs ++ devInputs ++ (with pkgs; [
              # Additional dev tools
              bear  # For compile_commands.json
              ccache
              ninja
            ]);
            
            shellHook = ''
              echo "DCF - DeMoD Communications Framework v5.2.0"
              echo "==========================================="
              echo ""
              echo "Build commands:"
              echo "  mkdir build && cd build"
              echo "  cmake .. -GNinja -DCMAKE_BUILD_TYPE=Debug"
              echo "  ninja"
              echo "  ctest --output-on-failure"
              echo ""
              echo "Nix build variants:"
              echo "  nix build              # Static lib + LTO (default)"
              echo "  nix build .#dcf-shared # Shared lib (uses clang)"
              echo "  nix build .#dcf-debug  # Debug build"
              echo "  nix build .#dcf-sanitized # ASan/UBSan"
              echo "  nix build .#dcf-coverage  # Code coverage"
              echo ""
              
              # Setup ccache
              export CCACHE_DIR="$PWD/.ccache"
              export CC="ccache gcc"
              export CXX="ccache g++"
            '';
            
            # Environment variables for development
            CMAKE_EXPORT_COMPILE_COMMANDS = "ON";
            ASAN_OPTIONS = "detect_leaks=1";
            UBSAN_OPTIONS = "print_stacktrace=1";
          };
          
          # Minimal shell for just building
          minimal = pkgs.mkShell {
            name = "dcf-minimal";
            packages = buildInputs;
          };
          
          # Shell with clang instead of gcc
          clang = pkgs.mkShell {
            name = "dcf-clang";
            packages = buildInputs ++ devInputs ++ [ pkgs.llvmPackages.clang ];
            
            shellHook = ''
              export CC=clang
              export CXX=clang++
              echo "Using Clang: $(clang --version | head -1)"
            '';
          };
          
          # Shell for fuzzing
          fuzz = pkgs.mkShell {
            name = "dcf-fuzz";
            packages = buildInputs ++ [
              pkgs.llvmPackages.clang
              pkgs.llvmPackages.libfuzzer
              pkgs.afl
            ];
            
            shellHook = ''
              export CC=clang
              export CXX=clang++
              echo "Fuzzing environment ready"
              echo "Use AFL or libFuzzer for fuzz testing"
            '';
          };
        };
        
        # Checks (run with `nix flake check`)
        checks = {
          # Run tests
          tests = dcf;
          
          # Run with sanitizers
          sanitizers = dcf-sanitized;
          
          # Format check
          format = pkgs.runCommand "dcf-format-check" {
            nativeBuildInputs = [ pkgs.clang-tools ];
            src = ./.;
          } ''
            cd $src
            find include src -name '*.c' -o -name '*.h' | \
              xargs clang-format --dry-run --Werror
            touch $out
          '';
          
          # Static analysis
          cppcheck = pkgs.runCommand "dcf-cppcheck" {
            nativeBuildInputs = [ pkgs.cppcheck ];
            src = ./.;
          } ''
            cd $src
            cppcheck --enable=all --error-exitcode=1 \
              --suppress=missingIncludeSystem \
              --suppress=unusedFunction \
              -I include src
            touch $out
          '';
        };
        
        # Apps (runnable binaries)
        apps = {
          example = {
            type = "app";
            program = "${dcf}/bin/example_basic";
          };
          
          tests = {
            type = "app";
            program = "${dcf}/bin/dcf_tests";
          };
        };
        
        # Overlay for use in other flakes
        overlays.default = final: prev: {
          dcf = dcf;
        };
      }
    ) // {
      # Templates for new projects using DCF
      templates = {
        default = {
          path = ./templates/basic;
          description = "Basic DCF project template";
        };
      };
    };
}
