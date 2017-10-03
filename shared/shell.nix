{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../../reflex-platform {}
, ghc ? reflex-platform.ghc }:

let

  inherit (nixpkgs) pkgs;

  reflex-websocket-interface-shared = ghc.callPackage ../reflex-websocket-interface/shared {};

  drv = ghc.callPackage (ghc.haskellSrc2nix {
  name = "fetchappdata-shared";
  src = ./.;}) {
    inherit reflex-websocket-interface-shared;
    };
in if pkgs.lib.inNixShell then drv.env else drv

