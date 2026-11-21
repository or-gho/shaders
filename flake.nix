{
  description = "Shaders of OCaml: typed shader eDSL for OpenGL and WebGPU";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, opam-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        opam = opam-nix.lib.${system};

        # Packages from opam, pinned to the versions in your opam file.
        scope = opam.buildOpamProject' { } ./. {
          ocaml-base-compiler = "*";
          dune = "*";
          ppxlib = "*";
          tsdl = "*";
          tgls = "*";
        };

        ocamlPkgs = scope;

        systemLibs = with pkgs; [
          SDL2
          libGL
          libGLU
          mesa
          pkg-config
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          inputsFrom = [ ];
          packages = [
            ocamlPkgs.ocaml
            ocamlPkgs.dune
            ocamlPkgs.ppxlib
            ocamlPkgs.tsdl
            ocamlPkgs.tgls
            pkgs.ocamlPackages.ocaml-lsp or null
            pkgs.ocamlformat or null
            pkgs.naga or null
          ] ++ systemLibs;

          # Lets pkg-config and the linker find SDL2 and libGL in the Nix store.
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.libGL pkgs.SDL2 ];

          shellHook = ''
            echo "Shaders of OCaml dev shell"
            echo "Run: dune build && dune test && dune exec shaders_of_ocaml"
          '';
        };

        packages.default = ocamlPkgs.shaders_of_ocaml or null;
      });
}