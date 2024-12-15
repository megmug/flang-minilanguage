{ pkgs ? import ( builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs/";
    ref = "nixos-24.11";
    rev = "a0f3e10d94359665dba45b71b4227b0aeb851f8e";
} ) {} }:

pkgs.stdenv.mkDerivation {
    name = "flang";

    buildInputs = [
        pkgs.ghc
	    pkgs.haskellPackages.haskell-language-server
	    pkgs.stack
    ];
}