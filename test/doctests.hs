import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "-XOverloadedStrings"
    , "-XDataKinds"
    , "-XFlexibleContexts"
    , "-XGADTs"
    , "-XLambdaCase"
    , "-XPolyKinds"
    , "-XRankNTypes"
    , "-XScopedTypeVariables"
    , "-XTypeApplications"
    , "-XTypeFamilies"
    , "-XTypeOperators"
    , "src/Version.hs"
    , "src/GH.hs"
    , "src/Time.hs"
    ]
