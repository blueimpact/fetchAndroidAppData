{ nixpkgs ? import <nixpkgs> {} }:

let

  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghc;
  hsPkgs = pkgs.haskellPackages;
  stdenv = pkgs.stdenv;

  pretty-simple = hsPkgs.callPackage ../pretty-simple/default.nix {};
  req = hsPkgs.callPackage ../req/default.nix {};
  drv = hsPkgs.callPackage (hsPkgs.haskellSrc2nix {
  name = "fetchappdata";
  src = ./.;}) {
    inherit pretty-simple req;
    };
in if pkgs.lib.inNixShell then drv.env else drv

