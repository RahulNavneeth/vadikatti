{
	description = "Vadikatti : Spam filter";
	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = {self, nixpkgs, flake-utils}:
	flake-utils.lib.eachDefaultSystem(system:
		let
			pkgs = nixpkgs.legacyPackages.${system};
		in {
			devShells.default = pkgs.mkShell {
				packages = with pkgs; [
					ghc
				 	cabal-install
					zsh
					gnumake
				];

				shellHook = ''
					exec ${pkgs.zsh}/bin/zsh
				'';
			};
		});
}
