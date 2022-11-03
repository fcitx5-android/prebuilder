with import <nixpkgs> { };

mkShell {
  buildInputs = [
    haskell-language-server
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [ shake ]))
  ];
}
