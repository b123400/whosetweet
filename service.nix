let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  fetchgit = pkgs.fetchgit;
  ghc = pkgs.haskell.compiler.ghc7102;
  makeWrapper = pkgs.makeWrapper;
  stack = pkgs.stack;

  whosetweet = import ./default.nix {
    inherit stdenv fetchgit makeWrapper stack;
  };


in {

  networking.firewall.allowedTCPPorts = [ 8888 ];

  systemd.services.whosetweet = {
    wantedBy = [ "multi-user.target" ];
    environment = {
      PORT = "8888";
      TWITTER_CLIENT_ID = "AAAAAAA";
      TWITTER_CLIENT_SECRET = "BBBBBBB";
      APPROOT = "http://whosetweet.b123400.net";
    };
    serviceConfig = {
      Type = "simple";
      ExecStart = ''
        ${whosetweet}/bin/whosetweet
      '';
      ExecStop = ''
      '';
      User = "whosetweet-user";
      WorkingDirectory = ''/var/whosetweet'';
    };
    path = [];
  };
}
