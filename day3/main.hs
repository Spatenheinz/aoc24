#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.regex])" -i runghc

{-# LANGUAGE  QuasiQuotes #-}
import Text.RE.TDFA.String

test = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

reg s = s *=~ [re|mul\(([0-9]{1,3}),([0-9]{1,3})\)|]

myMatch = allMatches . reg
