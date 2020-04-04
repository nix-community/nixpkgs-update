{ mkDerivation, aeson, base, bytestring, conduit, containers
, cryptohash-sha256, directory, doctest, errors, filepath, github
, hpack, hspec, hspec-discover, http-client-tls, http-conduit
, iso8601-time, lifted-base, mtl, neat-interpolation
, optparse-applicative, parsec, parsers, partial-order, polysemy
, polysemy-plugin, regex-applicative-text, servant, servant-client
, sqlite-simple, stdenv, template-haskell, temporary, text, time
, transformers, typed-process, unix, unordered-containers, vector
, versions, xdg-basedir, zlib
}:
mkDerivation {
  pname = "nixpkgs-update";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring conduit containers cryptohash-sha256
    directory errors filepath github http-client-tls http-conduit
    iso8601-time lifted-base mtl neat-interpolation
    optparse-applicative parsec parsers partial-order polysemy
    polysemy-plugin regex-applicative-text servant servant-client
    sqlite-simple template-haskell temporary text time transformers
    typed-process unix unordered-containers vector versions xdg-basedir
    zlib
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring conduit containers cryptohash-sha256
    directory errors filepath github http-client-tls http-conduit
    iso8601-time lifted-base mtl neat-interpolation
    optparse-applicative parsec parsers partial-order polysemy
    polysemy-plugin regex-applicative-text servant servant-client
    sqlite-simple template-haskell temporary text time transformers
    typed-process unix unordered-containers vector versions xdg-basedir
    zlib
  ];
  testHaskellDepends = [
    aeson base bytestring conduit containers cryptohash-sha256
    directory doctest errors filepath github hspec hspec-discover
    http-client-tls http-conduit iso8601-time lifted-base mtl
    neat-interpolation optparse-applicative parsec parsers
    partial-order polysemy polysemy-plugin regex-applicative-text
    servant servant-client sqlite-simple template-haskell temporary
    text time transformers typed-process unix unordered-containers
    vector versions xdg-basedir zlib
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/ryantm/nixpkgs-update#readme";
  description = "Tool for semi-automatic updating of nixpkgs repository";
  license = stdenv.lib.licenses.cc0;
}
