{ pkgs ? import ( builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs/";
    ref = "nixos-24.11";
    rev = "3ffbbdbac0566a0977da3d2657b89cbcfe9a173b";
} ) {} }:

pkgs.mkShell {
    buildInputs = [
        pkgs.ghc
	    pkgs.haskellPackages.haskell-language-server
        pkgs.haskellPackages.hspec-discover
	    pkgs.stack
    ];

    shellHook = ''
        alias flang-tokenize='stack run flang-tokenize'
        alias flang-tokenize-debug='stack run flang-tokenize-debug'
        alias flang-parse='stack run flang-parse'
        alias flang-parse-debug='stack run flang-parse-debug'
        alias flang-typify='stack run flang-typify'
        alias flang-typify-debug='stack run flang-typify-debug'
        alias flang-rewrite='stack run flang-rewrite'
        alias flang-rewrite-debug='stack run flang-rewrite-debug'
        alias flang-compile='stack run flang-compile'
        alias flang-compile-debug='stack run flang-compile-debug'
        alias flang-run='stack run flang-run'
        alias flang-run-debug='stack run flang-run-debug'
        alias flang-run-vmcode='stack run flang-run-vmcode'
        alias flang-run-vmcode-debug='stack run flang-run-vmcode-debug'
    '';
}