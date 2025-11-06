{
  description = "StreamDB - A lightweight, thread-safe embedded database";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.stdenv.mkDerivation rec {
          pname = "streamdb";
          version = "0.1.0"; # Initial version; update as needed

          src = ./.; # Point to local directory containing source files

          buildInputs = [ pkgs.util-linux.lib ]; # For libuuid

          buildPhase = ''
            $CC -c streamdb.c -o streamdb.o
            $CC -c libstreamdb_wrapper.c -o wrapper.o
            ar rcs libstreamdb.a streamdb.o wrapper.o
          '';

          installPhase = ''
            mkdir -p $out/lib $out/include
            cp libstreamdb.a $out/lib/
            cp streamdb.h $out/include/
            cp libstreamdb_wrapper.h $out/include/
          '';

          meta = with pkgs.lib; {
            description = "StreamDB - A lightweight, thread-safe embedded database using reverse trie";
            license = licenses.lgpl21Plus;
            platforms = platforms.unix;
            maintainers = [ { name = "DeMoD LLC"; } ];
          };
        };
      });
}
