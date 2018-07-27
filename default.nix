{ pkgs ? import <nixpkgs> {} }:
with (import (pkgs.fetchFromGitHub  {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "f682ff93a2778f101d93b68c97278f902523758a";
  sha256 = "0nrjfia5cgc5hac9m6hkn052p1d5bjawkrjjxwjnjxcxchvld6li";
}) {});
let haskpkgs = haskellPackages.extend (self: super: {
  some-board-game-rules = self.callCabal2nix "some-board-game-rules" ./. {};
});
in haskpkgs // haskpkgs.some-board-game-rules
