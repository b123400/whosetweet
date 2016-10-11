with (import <nixpkgs> {});

{ stdenv,
  fetchgit,
  makeWrapper,
  stack }:

let
  ghc = haskell.compiler.ghc7102;
in
haskell.lib.buildStackProject {
  name = "whosetweet";
  buildInputs = [ zlib stack makeWrapper ghc ];

  src = fetchgit {
    url = git://github.com/b123400/whosetweet;
    rev = "76649f2e06210b988abc77d7c329c0b45755c3fa";
    sha256 = "1fl2a5vwbfvarirnkmdx6h553i9hl0zs77k8qg9l1ka7f63mwy4b";
  };

  configurePhase = ''
    env
    export HOME=$NIX_BUILD_TOP/fake-home
    mkdir -p fake-home
    export STACK_ROOT=$NIX_BUILD_TOP/.stack
  '';

  installPhase = ''
    mkdir -p $out/bin
    stack --local-bin-path=$out/bin build --copy-bins
    cp -R $src $out/src
  '';
}
