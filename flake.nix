{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      rust-overlay,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      (
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ rust-overlay.overlays.default ];
          };

          rustToolchain = pkgs.rust-bin.stable.latest.default.override {
            extensions = [
              "rust-src"
              "rustfmt"
              "clippy"
              "rust-analyzer"
            ];
          };

          libs = with pkgs; [
          ];

          devTools = with pkgs; [
            gnumake
            pkg-config
            rustToolchain
          ];
        in
        {
          devShells.default = pkgs.mkShell {
            name = "dev-environment";

            buildInputs = devTools;
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
          };
        }
      )
    );
}
