{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Python";
    buildInputs = [
        (python37.withPackages(ps: with ps; [
            matplotlib
            numpy
            flake8
        ]))
        (haskell.packages.ghc864.ghcWithPackages (pkgs: [
            pkgs.random
            pkgs.tf-random
            pkgs.hlint
            pkgs.hoogle
            pkgs.HUnit
        ]))
        wget
    ];
    shellHook = ''
        if [ $(uname -s) = "Darwin" ]; then
            alias ls='ls --color=auto'
            alias ll='ls -al'
        fi
        alias flake8="flake8 --ignore E124,E128,E201,E203,E241,E731,W503"
    '';
}
