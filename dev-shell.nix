let                                
   stable = import (builtins.fetchTarball {                                
      name = "nixos-21.05";                                
      url = "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";                                
      sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";                                
   }) {};                                
in                                
  stable.mkShell {                                
    buildInputs =
      [
         stable.ormolu
         stable.kind
         stable.kubernetes-helm
         stable.envsubst
     ];                                
  }      
