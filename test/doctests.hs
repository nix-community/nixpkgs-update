import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "-XOverloadedStrings", "src/Version.hs", "src/GH.hs"]
