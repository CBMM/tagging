let
  pkgs = import <nixpkgs> {};
  pkgs2 =  import <nixos> {};
  pypkgs = pkgs.python27Packages.boto;
in
{ stdenv ? pkgs.stdenv , python ? pkgs.python, pyirc ? pkgs.pythonIRClib , ffmpeg ? pkgs.ffmpeg }:

stdenv.mkDerivation {
  name = "python-nix";
  version = "0.1.0.0";
  src = ./.;
  buildInputs = [ python pyirc pypkgs ffmpeg ];
}
