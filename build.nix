{ compiler ? "ghc8103"
, ihp
, haskellDeps ? (p: [])
, otherDeps ? (p: [])
, projectPath
}:

let
    pkgs = import "${toString projectPath}/Config/nix/nixpkgs-config.nix" { ihp = ihp; };
    ghc = pkgs.haskell.packages.${compiler};
    allHaskellPackages = ghc.ghcWithPackages
    (p: builtins.concatLists [ 
      [p.haskell-language-server] 
      (haskellDeps p) 
    ]);
    allNativePackages = builtins.concatLists [
      (otherDeps pkgs)
      [pkgs.postgresql]
    ];
in
    pkgs.stdenv.mkDerivation {
        name = "app";
        buildPhase = ''
          make -f ${ihp}/lib/IHP/Makefile.dist -B build/bin/RunOptimizedProdServer
          make -f ${ihp}/lib/IHP/Makefile.dist -B build/bin/RunJobs
        '';
        installPhase = ''
          mkdir -p $out
          cp -r build/bin $out/bin

          mkdir -p $out/static
          cp -r ./static $out

          mkdir -p $out/Config
          cp -r ./Config $out
        '';
        dontFixup = true;
        src = (import <nixpkgs> {}).nix-gitignore.gitignoreSource [] projectPath;
        buildInputs = builtins.concatLists [[allHaskellPackages] allNativePackages];
        shellHook = "eval $(egrep ^export ${allHaskellPackages}/bin/ghc)";
    }
