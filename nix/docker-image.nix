{ pkgs, project, version, ... }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/paolino/keri-coop";
  tag = version;
  config = {
    EntryPoint = [
      "keri-coop-server"
      "--static-dir"
      "/public"
      "--db"
      "/data/keri-coop.db"
    ];
    ExposedPorts = { "3001/tcp" = { }; };
    Volumes = { "/data" = { }; };
  };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      project.packages.keri-coop-server
      (pkgs.runCommand "client-bundle" { } ''
        mkdir -p $out/public
        cp -r ${../client/dist}/* $out/public/
      '')
    ];
  };
}
