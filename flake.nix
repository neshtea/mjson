{
  description = "Dev-environment for the Mjson library.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = with inputs.flake-utils.lib.system; [
        aarch64-darwin
        x86_64-linux
      ];
    in inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ inputs.ocaml-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };

        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_0;
      in {

        formatter = pkgs.nixfmt;
        devShells = {
          default = pkgs.mkShell {
            nativeBuildInputs = [
              pkgs.gnumake
              pkgs.dune_3
              pkgs.dune-release
              ocamlPackages.opam
              ocamlPackages.merlin

              # ocamlStreaming
              ocamlPackages.ocamlformat
              ocamlPackages.ocaml
              ocamlPackages.utop
              ocamlPackages.ocaml-lsp
              ocamlPackages.ppx_deriving
              ocamlPackages.ppx_deriving_yojson
              ocamlPackages.yojson
              ocamlPackages.alcotest
              ocamlPackages.odoc
            ];
          };
        };
      });
}
