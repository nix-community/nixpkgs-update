{ mkDerivation, aeson, base, bytestring, conduit, containers
, cryptohash-sha256, directory, doctest, errors, filepath, github
, hex, hpack, http-conduit, iso8601-time, lifted-base, mtl
, neat-interpolation, optparse-applicative, parsec, parsers
, partial-order, polysemy, regex-applicative-text, sqlite-simple
, stdenv, template-haskell, temporary, text, time, transformers
, typed-process, unix, vector, versions, xdg-basedir, zlib
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
    parsers partial-order polysemy regex-applicative-text sqlite-simple
    template-haskell temporary text time transformers typed-process
    unix vector versions xdg-basedir zlib
  ];
  testHaskellDepends = [
    aeson base bytestring conduit containers cryptohash-sha256
    directory doctest errors filepath github hex http-conduit
    iso8601-time lifted-base mtl neat-interpolation
    optparse-applicative parsec parsers partial-order polysemy
    regex-applicative-text sqlite-simple template-haskell temporary
    text time transformers typed-process unix vector versions
    xdg-basedir zlib
  ];
  prePatch = "hpack";
  homepage = "https://github.com/ryantm/nixpkgs-update#readme";
  description = "Tool for semi-automatic updating of nixpkgs repository";
  license = stdenv.lib.licenses.cc0;
}
