let
  ihp = builtins.fetchGit {
    url = "https://github.com/digitallyinduced/ihp.git";
    ref = "v0.11.0";
  };
in
  import ./build.nix {
    ihp = ihp;
    haskellDeps = p: with p; [
      cabal-install
      base
      wai
      text
      hlint
      req
      p.ihp
      hspec
    ];
    otherDeps = p: with p; [];
    projectPath = ./.;
  }
