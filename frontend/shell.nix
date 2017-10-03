{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../reflex-platform {}
, ghc ? reflex-platform.ghcjs }:

let

  inherit (nixpkgs) pkgs;

  reflex-websocket-interface-shared = ghc.callPackage ../reflex-websocket-interface/shared {};
  reflex-websocket-interface = ghc.callPackage ../reflex-websocket-interface/reflex {inherit reflex-websocket-interface-shared;};

  reflex-material-bootstrap = ghc.callPackage ../reflex-material-bootstrap {};

  fetchappdata-shared = ghc.callPackage ../shared
    {inherit reflex-websocket-interface-shared; };

  pretty-simple = ghc.callPackage ../../pretty-simple/default.nix {};
  drv = ghc.callPackage (ghc.haskellSrc2nix {
  name = "fetchappdata-frontend";
  src = ./.;}) {
    inherit pretty-simple reflex-material-bootstrap
      reflex-websocket-interface-shared reflex-websocket-interface fetchappdata-shared;
    };
in if pkgs.lib.inNixShell then drv.env else drv

