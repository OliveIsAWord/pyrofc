{

inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";

outputs = { self, nixpkgs }: let
  supportedSystems = nixpkgs.lib.systems.flakeExposed;
  allSystems = output: nixpkgs.lib.genAttrs supportedSystems
    (system: output nixpkgs.legacyPackages.${system});
in {
  packages = allSystems (pkgs: {
    default = pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
      pname = "bee";
      version = "0.0.0";
      src = ./.;
      nativeBuildInputs = [ pkgs.ghc ];
      buildPhase = "ghc main.hs";
      installPhase = "install -D main $out/bin/${finalAttrs.pname}";
    });
  });

  devShells = allSystems (pkgs: {
    default = pkgs.mkShellNoCC {
      packages = with pkgs; [
        ghcid
        hlint
      ];
      nativeBuildInputs = [ pkgs.ghc ];
    };
  });
};

}