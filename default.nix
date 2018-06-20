{ nixpkgs ? (import ~/.nix-overlay/nixpkgs {}) }:

let
  #haskellPackages = nixpkgs.haskell.packages.ghc841;
  haskellPackages = nixpkgs.haskellPackages;
in rec {
  auto-push = haskellPackages.callCabal2nix "auto-push" ./. {};
  circlehs = haskellPackages.callCabal2nix "circlehs" ./circlehs {};
  jenkins = haskellPackages.callCabal2nix "jenkins" ./jenkins {};
}