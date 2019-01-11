{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Python";
    buildInputs = [ python36
                    python36Packages.matplotlib
                    python36Packages.numpy
                    python36Packages.flake8
                    wget
                  ];
    shellHook = ''
        alias flake8="flake8 --ignore E124,E128,E201,E203,E241,E731,W503"

        dir="data"
        if [ ! -e ./$dir/ ]; then
            mkdir $dir
        fi
    '';
}
