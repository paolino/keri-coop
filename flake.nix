{
  description = "keri-purs — PureScript KERI library";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, purescript-overlay, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          overlays = [ purescript-overlay.overlays.default ];
          inherit system;
        };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.purs
            pkgs.spago-unstable
            pkgs.purs-tidy
            pkgs.nodejs_20
            pkgs.just
            pkgs.nixfmt-classic
          ];
        };
      });
}
