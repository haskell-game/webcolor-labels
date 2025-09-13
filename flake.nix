# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "webcolor-labels";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self {
            # Dependency overrides go here
          };

        packages."${packageName}-dist" =
          with pkgs.haskell.lib;
          let distPackage = self.packages.${system}.default;
          in pkgs.runCommand "pack-${packageName}-dist" {} ''
            mkdir $out
            mkdir -m 777 $out/packages $out/docs
            cp -r ${sdistTarball distPackage}/${distPackage.name}.tar.gz $out/packages
            cp -r ${documentationTarball distPackage}/${distPackage.name}-docs.tar.gz $out/docs
          '';

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
            haskellPackages.cabal-fmt
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
