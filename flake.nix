{
  description = "Flake for developing the SwayWM REPL.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        sbcl = pkgs.sbcl.withPackages (
          ps: with ps; [
            clingon
            closer-mop
            fiveam
          ]
        );
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.gnumake
            pkgs.asdf
            pkgs.roswell
            sbcl
          ];
        };
      }
    );
}
