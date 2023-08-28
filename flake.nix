{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.systems.url = "github:nix-systems/default";

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , systems
    ,
    }:
    flake-utils.lib.eachSystem (import systems)
      (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      with pkgs;{
        devShells.default = pkgs.mkShell {
          buildInputs = [
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-language-server

            nodejs
            nodePackages.pnpm
            nodePackages.typescript
            nodePackages.typescript-language-server
            nodePackages.vscode-langservers-extracted
          ];
        };

        formatter = nixpkgs-fmt;
      });
}
