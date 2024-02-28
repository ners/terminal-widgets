{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    haskell-terminal = {
      url = "github:ners/haskell-terminal/fix-virtual-terminal-erase";
      flake = false;
    };
    text-rope-zipper = {
      url = "github:ners/text-rope-zipper";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      hsSrc = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "md" ] || file.type == "directory") ./.;
      };
      pname = "terminal-widgets";
      src = hsSrc ./.;
      ghcs = [ "ghc94" "ghc96" "ghc98" ];
      overlay = lib.composeManyExtensions [
        inputs.text-rope-zipper.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeExtensions
              prev.haskell.packageOverrides
              (hfinal: hprev: {
                terminal = hfinal.callCabal2nix "terminal" inputs.haskell-terminal { };
                "${pname}" = hfinal.callCabal2nix pname src { };
              });
          };
        })
      ];
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps =
            lib.filterAttrs (ghc: _: elem ghc ghcs) pkgs.haskell.packages
            // { default = pkgs.haskellPackages; };
          allPackages =
            pkgs.symlinkJoin {
              inherit name;
              paths = map (hp: hp.${pname}) (attrValues hps);
            };
          docs = pkgs.haskell.lib.documentationTarball hps.default.${pname};
          sdist = hps.default.cabalSdist { name = "${pname}.tar.gz"; inherit src; };
          inherit (hps.default.${pname}) name;
          default = pkgs.runCommand name { } ''
            mkdir $out
            cd $out
            mkdir docs sdist
            ln -s ${allPackages} ${name}
            ln -s ${docs}/*.tar.gz docs/
            ln -s ${sdist} sdist/${name}.tar.gz
          '';
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system} = { inherit default; };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: [ ps.${pname} ];
                nativeBuildInputs = with hp; [
                  pkgs'.haskellPackages.cabal-install
                  pkgs'.haskellPackages.fourmolu
                  haskell-language-server
                ];
              };
            });
        }
      ) // {
      overlays.default = overlay;
    };
}
