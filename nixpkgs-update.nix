{ mkDerivation, aeson, base, bytestring, conduit, containers
, cryptohash-sha256, directory, doctest, errors, filepath, github
, hex, hpack, http-conduit, iso8601-time, lifted-base, mtl
, neat-interpolation, optparse-applicative, parsec, parsers
, polysemy, regex-applicative-text, shelly, stdenv
, template-haskell, text, time, transformers, typed-process, unix
, vector, xdg-basedir, zlib
}:
mkDerivation {
  pname = "nixpkgs-update";
  version = "0.2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring conduit containers cryptohash-sha256
    directory errors filepath github hex http-conduit iso8601-time
    lifted-base mtl neat-interpolation optparse-applicative parsec
    parsers polysemy regex-applicative-text shelly template-haskell
    text time transformers typed-process unix vector xdg-basedir zlib
  ];
  testHaskellDepends = [
    aeson base bytestring conduit containers cryptohash-sha256
    directory doctest errors filepath github hex http-conduit
    iso8601-time lifted-base mtl neat-interpolation
    optparse-applicative parsec parsers polysemy regex-applicative-text
    shelly template-haskell text time transformers typed-process unix
    vector xdg-basedir zlib
  ];
  prePatch = "hpack";
  homepage = "https://github.com/ryantm/nixpkgs-update#readme";
  description = "Tool for semi-automatic updating of nixpkgs repository";
  license = stdenv.lib.licenses.cc0;
}
