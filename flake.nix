{
  description = "A pure OCaml HTTP client for Riot";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    castore = {
      url = "github:suri-framework/castore";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    minttea = {
      url = "github:leostera/minttea";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rio = {
      url = "github:riot-ml/rio";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    riot = {
      url = "github:riot-ml/riot";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.castore.follows = "castore";
      inputs.minttea.follows = "minttea";
      inputs.rio.follows = "rio";
      inputs.telemetry.follows = "telemetry";
    };

    telemetry = {
      url = "github:leostera/telemetry";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    trail = {
      url = "github:suri-framework/trail";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.minttea.follows = "minttea";
      inputs.rio.follows = "rio";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          inherit (pkgs) ocamlPackages mkShell;
          inherit (ocamlPackages) buildDunePackage;
          version = "0.0.1+dev";
        in
          {
            devShells = {
              default = mkShell {
                buildInputs = with ocamlPackages; [
                  dune_3
                  ocaml
                  utop
                  ocamlformat
                ];
                inputsFrom = [
                  self'.packages.default
                ];
                packages = builtins.attrValues {
                  inherit (ocamlPackages) ocaml-lsp ocamlformat-rpc-lib;
                };
              };
            };

            packages = {
              http = buildDunePackage {
                version = "v6.0.0_beta2";
                pname = "http";
                src = builtins.fetchGit {
                  url = "git@github.com:mirage/ocaml-cohttp.git";
                  rev = "5da40ec181f8afb2ba6788d20c4d35bc8736c649";
                  ref = "refs/tags/v6.0.0_beta2";
                };
              };

              default = buildDunePackage {
                inherit version;
                pname = "blink";
                propagatedBuildInputs = with ocamlPackages; [
                  inputs'.trail.packages.default
                  angstrom
                  inputs'.castore.packages.default
                  cohttp
                  httpaf
                  faraday
                  self'.packages.http
                  (mdx.override {
                    inherit logs;
                  })
                  mirage-crypto-rng
                  odoc
                  inputs'.riot.packages.default
                  x509
                  digestif
                ];
                src = ./.;
              };
            };
          };
    };
}
