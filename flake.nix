{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, fenix, utils }: 
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        rust = fenix.packages."${system}";
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
          
          ];
          packages = with pkgs; [
            rust.stable.toolchain
          ];
          buildInputs = with pkgs; [
          
          ];
        };
      }
    );
}
