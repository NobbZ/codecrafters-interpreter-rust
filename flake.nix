{
  outputs = {parts, ...} @ inputs:
    parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux"];

      perSystem = {
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        pkgsWithOverlays = (inputs.nixpkgs.legacyPackages.${system}.extend inputs.rust-overlay.overlays.default).extend inputs.cargo2nix.overlays.default;
        rustVersion = "1.82.0";
        rust = pkgs.rust-bin.stable.${rustVersion}.default;
      in {
        _module.args.pkgs = pkgsWithOverlays;

        formatter = pkgs.alejandra;

        legacyPackages = {inherit rust;};

        devShells.default = pkgs.mkShell {
          packages = builtins.attrValues {
            inherit (pkgs) cargo-nextest cargo-audit cargo-deny cargo-tarpaulin rust-analyzer;
            inherit (pkgs) nil pre-commit reuse;
            inherit (pkgs) watchexec;
            inherit rust;
          };
        };
      };
    };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";

    rust-overlay.url = "github:oxalica/rust-overlay";

    parts.url = "github:hercules-ci/flake-parts";

    cargo2nix.url = "github:cargo2nix/cargo2nix";
    cargo2nix.inputs.nixpkgs.follows = "nixpkgs";
    cargo2nix.inputs.rust-overlay.follows = "rust-overlay";

    systems.url = "github:nix-systems/default-linux";
    systems.flake = false;
  };
}
