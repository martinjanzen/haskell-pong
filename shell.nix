with import <nixpkgs> { };
let pong = haskellPackages.callCabal2nix "pong" ./. {};
in
haskellPackages.shellFor {
  packages = p: [ pong ];
  # extraDependencies = p: {
  #   libraryHaskellDepends = with p; [ ansi-terminal ];
  # };
  withHoogle = true;
  buildInputs = with haskellPackages; [
    haskell-language-server
    fourmolu
    cabal-install
  ] ++ (with pkgs; [
  ]);
  # Change the prompt to show that you are in a devShell
  # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
}
