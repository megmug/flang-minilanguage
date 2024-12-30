{ pkgs ? import ( builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs/";
    ref = "nixos-24.11";
    rev = "9f94733f93e4fe6e82f516efae007096e4ab5a21";
} ) {} }:

pkgs.mkShell {
    buildInputs = [
        pkgs.ghc
	    pkgs.haskellPackages.haskell-language-server
	    pkgs.stack
    ];
}