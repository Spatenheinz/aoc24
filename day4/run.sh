#!/usr/bin/env bash

nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [vector])" --run " ghc main.hs && ./main"
