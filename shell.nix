{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  nodeEnv = import ./nix/node-env.nix {
    inherit (pkgs) stdenv python2 utillinux runCommand writeTextFile;
    inherit nodejs;
  };
  extraNodePkgs = import ./nix/node-packages.nix {
    inherit fetchurl fetchgit; 
    inherit nodeEnv;
  };
in
stdenv.mkDerivation {
  name = "purescript-monadicvis";

  buildInputs = [
    purescript
    nodejs
    nodePackages.bower
    extraNodePkgs."d3-4.9.1"
    extraNodePkgs."d3-scale-chromatic-1.1.1"
    extraNodePkgs."preact-8.1.0"
    extraNodePkgs."preact-compat-3.16.0"
    extraNodePkgs."purescript-psa-0.5.1"
    extraNodePkgs."purs-loader-3.0.0"
    extraNodePkgs."rimraf-2.6.1"
    extraNodePkgs."webpack-2.6.1"
    extraNodePkgs."webpack-dev-server-2.4.5"
    extraNodePkgs."webpack-node-externals-1.6.0"
    extraNodePkgs."xhr2-0.1.4"
    extraNodePkgs."css-loader-0.28.4"
    extraNodePkgs."node-sass-4.5.3"
    extraNodePkgs."pulp-11.0.0"
    extraNodePkgs."sass-loader-6.0.5"
    extraNodePkgs."style-loader-0.17.0"
  ];
}

