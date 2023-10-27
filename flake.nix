{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      supportedSystems = with inputs.flake-utils.lib.system; [ aarch64-darwin ];
    in inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        inherit (inputs.gitignore.lib) gitignoreSource;
        overlays = [ inputs.ocaml-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };

        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_0;
      in {

        formatter = pkgs.nixfmt;
        devShells = {
          default = let
          in pkgs.mkShell {
            nativeBuildInputs = [
              pkgs.gnumake
              pkgs.dune_3
              pkgs.sqlite
			  pkgs.nodejs
			  ocamlPackages.opam

              # ocamlStreaming
              ocamlPackages.ocamlformat
              ocamlPackages.ocaml
              ocamlPackages.utop
              ocamlPackages.ocaml-lsp
              ocamlPackages.merlin
              ocamlPackages.ppx_deriving
              ocamlPackages.ppx_deriving_yojson
              ocamlPackages.lwt
              ocamlPackages.lwt_ppx
              ocamlPackages.caqti
              ocamlPackages.caqti-lwt
              ocamlPackages.caqti-driver-postgresql
              ocamlPackages.caqti-driver-sqlite3
              ocamlPackages.dream
              ocamlPackages.ptime
              ocamlPackages.tyxml
			  ocamlPackages.yojson
			  ocamlPackages.alcotest
			  ocamlPackages.odoc
            ];
          };
        };
      });
}
