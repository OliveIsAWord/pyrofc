{

description = "idk man";

inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";

outputs = { self, nixpkgs }: let
  supportedSystems = nixpkgs.lib.systems.flakeExposed;
  allSystems = output: nixpkgs.lib.genAttrs supportedSystems
    (system: output nixpkgs.legacyPackages.${system});
in {
  packages = allSystems (pkgs: {
    default = pkgs.rustPlatform.buildRustPackage {
      pname = "pyrofc";
      version = "0.1.0";
      src = ./.;
      cargoLock.lockFile = ./Cargo.lock;
    };
  });

  devShells = allSystems (pkgs: {
    default = pkgs.mkShellNoCC {
      packages = with pkgs; [
        cargo
        cargo-watch
        rustfmt
        clippy
      ];
    };
  });
};

}
