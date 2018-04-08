import Test.DocTest

main = doctest ["-isrc", "-XOverloadedStrings", "src/Utils.hs"]
