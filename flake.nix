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
        pkgs = import nixpkgs {
          inherit system;
          overlays = [overlay];
        };

        overlay = _: prev: {
          haskell = prev.haskell // {
            packages = pkgs.lib.mapAttrs overrideHaskellPackages prev.haskell.packages;
          };
        };

        overrideHaskellPackages = _: hp: hp.override {
          overrides = _: _: {
            "${packageName}" = hp.callCabal2nix packageName self {};
          };
        };

        haskellPackages = pkgs.haskellPackages;

        packageName = "webcolor-labels";

        mkPackage = _: hp: { "${packageName}" = hp."${packageName}";};
      in {

        overlays.default = overlay;

        packages = pkgs.lib.mapAttrs mkPackage pkgs.haskell.packages // {

          "${packageName}" = haskellPackages."${packageName}";

          default = self.packages.${system}."${packageName}";

          "${packageName}-dist" =
          with pkgs.haskell.lib;
          let distPackage = self.packages.${system}.default;
          in pkgs.runCommand "pack-${packageName}-dist" {} ''
            mkdir $out
            mkdir -m 777 $out/packages $out/docs
            cp -r ${sdistTarball distPackage}/${distPackage.name}.tar.gz $out/packages
            cp -r ${documentationTarball distPackage}/${distPackage.name}-docs.tar.gz $out/docs
          '';
        };

        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
            haskellPackages.cabal-fmt
          ];
          inputsFrom = [ self.defaultPackage.${system}.env ];
        };
        devShell = self.devShells.${system}.default;
      });
}
