{
  description = "Flake for developing the SwayWM REPL.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        sbcl' = pkgs.sbcl.withPackages (ps: with ps; [ fiveam ]);
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.gnumake pkgs.asdf pkgs.roswell sbcl' ];

          buildPhase = ''
            sbcl --eval '(load (sb-ext:posix-getenv "ASDF"))' \
                 --load swaywm-repl.asd
                 --eval '(asdf:make :swaywm-repl)'
                 --eval '(quit)            
          '';
        };
      });
}
