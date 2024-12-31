{ pkgs ? import ( builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs/";
    ref = "nixos-24.11";
    rev = "3ffbbdbac0566a0977da3d2657b89cbcfe9a173b";
} ) {} }:

pkgs.mkShell {
    buildInputs = [
        pkgs.ghc
	    pkgs.haskellPackages.haskell-language-server
	    pkgs.stack
    ];
}