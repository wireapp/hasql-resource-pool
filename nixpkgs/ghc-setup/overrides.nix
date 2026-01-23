{
    hsLib,
    withGMP
}:

(self: original: rec {
    # saves a lot of build time in non-critical test envs
    # scientific              = hsLib.dontCheck original.scientific;
    # haskell-language-server = hsLib.dontCheck original.haskell-language-server;
})
