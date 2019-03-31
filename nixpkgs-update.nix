{ mkDerivation, base, bytestring, containers, directory, doctest
, errors, filepath, github, hpack, lifted-base, mtl
, neat-interpolation, optparse-applicative, parsec, parsers
, regex-applicative-text, shelly, stdenv, template-haskell, text
, time, transformers, typed-process, unix, vector, xdg-basedir
}:
mkDerivation {
  pname = "nixpkgs-update";
  version = "0.2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring containers directory errors filepath github
    lifted-base mtl neat-interpolation optparse-applicative parsec
    parsers regex-applicative-text shelly template-haskell text time
    transformers typed-process unix vector xdg-basedir
  ];
  testHaskellDepends = [
    base bytestring containers directory doctest errors filepath github
    lifted-base mtl neat-interpolation optparse-applicative parsec
    parsers regex-applicative-text shelly template-haskell text time
    transformers typed-process unix vector xdg-basedir
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/ryantm/nixpkgs-update#readme";
  description = "Tool for semi-automatic updating of nixpkgs repository";
  license = stdenv.lib.licenses.publicDomain;
}
