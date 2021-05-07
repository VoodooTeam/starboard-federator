let
  ihp = builtins.fetchGit {
    url = "https://github.com/digitallyinduced/ihp.git";
    ref = "d871598f730a9188129dffd61426843ddbc4cf60";
  };
  unstable = import (builtins.fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) {};
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
    otherDeps = p: with p; [
      unstable.ormolu
      unstable.kind
      unstable.kubernetes-helm
    ];
    projectPath = ./.;
  }
