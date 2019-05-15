{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Python";
    buildInputs = [
        (python37.withPackages(ps: with ps; [
            flake8
            matplotlib
            numpy
            scikitlearn
        ]))
        (haskell.packages.ghc865.ghcWithPackages (pkgs: [
            pkgs.containers
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
        export WD=$(pwd)
        if [ ! -d pngs/ ]; then
            mkdir pngs/
        fi
    '';
}
