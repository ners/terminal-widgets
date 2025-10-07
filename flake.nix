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
      pname = "terminal-widgets";

      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "md" ]) root;
      };
      ghcsFor = pkgs: with lib; foldlAttrs
        (acc: name: hp':
          let
            hp = tryEval hp';
            version = getVersion hp.value.ghc;
            majorMinor = versions.majorMinor version;
            ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
          in
          if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.4" && versionOlder version "9.13"
          then acc // { ${ghcName} = hp.value; }
          else acc
        )
        { }
        pkgs.haskell.packages;
      hpsFor = pkgs: { default = pkgs.haskellPackages; } // ghcsFor pkgs;
      haskell-overlay = lib.composeManyExtensions [
        inputs.text-rope-zipper.overlays.haskell
        (hfinal: hprev: {
          terminal = hfinal.callCabal2nix "terminal" inputs.haskell-terminal { };
          ${pname} = hfinal.callCabal2nix pname (sourceFilter ./.) { };
        })
      ];
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = lib.composeManyExtensions [
            prev.haskell.packageOverrides
            haskell-overlay
          ];
        };
      };
    in
    {
      overlays = {
        default = overlay;
        haskell = haskell-overlay;
      };
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps = hpsFor pkgs;
          bins = pkgs.buildEnv {
            name = "${pname}-bins";
            paths = [ hps.default.${pname} ];
            pathsToLink = [ "/bin" ];
          };
          libs = pkgs.buildEnv {
            name = "${pname}-libs";
            paths = map (hp: hp.${pname}) (attrValues hps);
            pathsToLink = [ "/lib" ];
          };
          docs = pkgs.haskell.lib.documentationTarball hps.default.${pname};
          sdist = pkgs.haskell.lib.sdistTarball hps.default.${pname};
          docsAndSdist = pkgs.linkFarm "${pname}-docsAndSdist" { inherit docs sdist; };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.symlinkJoin {
            name = "${pname}-all";
            paths = [ bins libs docsAndSdist ];
            inherit (hps.default.syntax) meta;
          };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: [ hp.${pname} ];
                nativeBuildInputs = [
                  pkgs'.haskellPackages.cabal-install
                  hp.fourmolu
                ] ++ lib.optionals (lib.versionAtLeast hp.ghc.version "9.4") [
                  hp.haskell-language-server
                ];
              };
            });
        }
      );
}
