{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../reflex-platform {}
, ghc ? reflex-platform.ghc }:

let

  inherit (nixpkgs) pkgs;

  reflex-websocket-interface-shared = ghc.callPackage ../reflex-websocket-interface/shared {};
  reflex-websocket-interface-server = ghc.callPackage ../reflex-websocket-interface/server {inherit reflex-websocket-interface-shared;};

  fetchappdata-shared = ghc.callPackage ../shared
    {inherit reflex-websocket-interface-shared; };

  pretty-simple = ghc.callPackage ../../pretty-simple/default.nix {};
  req = ghc.callPackage ../../req/default.nix {};
  drv = ghc.callPackage (ghc.haskellSrc2nix {
  name = "fetchappdata";
  src = ./.;}) {
    inherit pretty-simple req fetchappdata-shared
    reflex-websocket-interface-shared reflex-websocket-interface-server;
    };
in if pkgs.lib.inNixShell then drv.env else drv

