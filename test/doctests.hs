import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "-XOverloadedStrings", "src/Utils.hs"]
