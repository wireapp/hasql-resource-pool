{   # To see the list of available compilers in the current nixpkgs snapshot, run 'make list-ghc-versions'
    haskellVersion  ? "9122"
,   haskellCompiler ? "ghc"
    # this affects static linking as well as licensing mode,
    # GMP assumes GPL license for the entire project if linked statically
,   withGMP         ? false
}:

let
    commonEnv       = import ./nixpkgs { supportedGhcVersions = [haskellVersion]; };
    pkgs                        = commonEnv.pkgs;
    hsLib                       = pkgs.haskell.lib;
    unbreak                     = hsLib.markUnbroken;
    addBuildDepends             = hsLib.addBuildDepends;
    disableSharedExecutables    = hsLib.disableSharedExecutables;
    dontCheck                   = hsLib.dontCheck;
    doJailbreak                 = hsLib.doJailbreak;
    justStaticExecutables       = hsLib.justStaticExecutables;
    overrideCabal               = hsLib.overrideCabal;
    disableFlag                 = hsLib.disableCabalFlag;
    ghcOverrides                = import ./nixpkgs/ghc-setup/overrides.nix { inherit hsLib; inherit withGMP; };
    haskellLibraries            = hackagePkgs: with hackagePkgs;
                                    [   cabal-install
                                        dotenv
                                    ];

    ghcEnv = commonEnv.ghcEnv
        {   pkgs                    = pkgs
        ;   haskellCompiler         = "${haskellCompiler}${haskellVersion}"
        ;   isHaskellWithGMP        = withGMP
        ;   haskellHackageOverrides = ghcOverrides
        ;   haskellLibraries        = hackagePkgs: with hackagePkgs;
                                        [   cabal-install
                                            # if hls is built from toplevel pkgs, it has to match the version of the project's GHC, otherwise VSCode plugin would complain
                                            # on version mismatch. Stylish Haskell is being put here to share the same pkg sets with hls
                                            haskell-language-server
                                            stylish-haskell
                                        ]
        ;
        };
    callCabal2Nix   = ghcEnv.ghcPkgSetWithOverrides.callCabal2nix;
    ghc             = ghcEnv.ghc;
    macOsDeps       = with pkgs; lib.optionals stdenv.isDarwin
                        [   darwin.apple_sdk.frameworks.CoreServices
                            darwin.apple_sdk.frameworks.ApplicationServices
                        ];

    devEnv  = pkgs.mkShellNoCC {
        # Sets the build inputs, i.e. what will be available in our
        # local environment.
        nativeBuildInputs = with pkgs; [
            cacert
            glibcLocales
            gnumake
            pre-commit
            postgresql
            ghcEnv.ghc
            zlib
            pkg-config
        ];
        buildInputs = with pkgs; [
            postgresql_18
        ];
        shellHook = ''
            export PROJECT_PLATFORM="${builtins.currentSystem}"
            export LANG=en_GB.UTF-8

            # https://cabal.readthedocs.io/en/3.4/installing-packages.html#environment-variables
            export CABAL_DIR=$PWD/.local/${builtins.currentSystem}/cabal

            # symbolic link to Language Server to satisfy VSCode Haskell plugins
            ln -s -f `which haskell-language-server-wrapper` $PWD/hls.exe
        '';
    };

in

{
    inherit devEnv;
}
