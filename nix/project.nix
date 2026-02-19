{ pkgs, ... }:

let
  indexState = "2026-02-01T00:00:00Z";

  shell = { pkgs, ... }: {
    tools = {
      cabal = { index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
      fourmolu = { index-state = indexState; };
    };
    buildInputs = [
      pkgs.just
      pkgs.nixfmt-classic
      # PureScript tooling
      pkgs.purs
      pkgs.spago-unstable
      pkgs.purs-tidy-bin.purs-tidy-0_10_0
      pkgs.esbuild
      pkgs.nodejs_20
    ];
    shellHook = ''
      echo "keri-coop dev shell — Haskell + PureScript"
    '';
  };

  mkProject = { lib, pkgs, ... }: {
    name = "keri-coop";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };
    modules = [ ];
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.keri-coop-server =
    project.hsPkgs.keri-coop.components.exes.keri-coop-server;
}
