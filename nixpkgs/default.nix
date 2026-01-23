{   supportedGhcVersions ?  [ "9122" ]
}:

let

common-src = builtins.fetchTarball {
    name = "common-2026-01-23";
    url = https://github.com/avanov/nix-common/archive/255c27549af6dbd343d7aa9af701b1e9aea3470b.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "sha256:11spp8q16zzs4ap8gvq2sc1p8gjxhzmswxnfnfcv5mh6qf1m7ll5";
};

overlays    = import ./overlays.nix {};
nixpkgsDist = (import common-src { projectOverlays = [ overlays.globalPackageOverlay ]; inherit supportedGhcVersions; });
pkgs        = nixpkgsDist.pkgs;
ghcEnv      = import "${common-src}/ghc-env.nix";  # will have to be called with required arguments by the consumer

in

{
    inherit nixpkgsDist;  # for repl testing
    inherit pkgs;
    inherit ghcEnv;
}
