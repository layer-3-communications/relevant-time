nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ aeson base chronos text torsor ])"
