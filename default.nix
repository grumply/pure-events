{ mkDerivation, base, containers, pure-core, pure-default
, pure-lifted, pure-txt, stdenv
}:
mkDerivation {
  pname = "pure-events";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers pure-core pure-default pure-lifted pure-txt
  ];
  homepage = "github.com/grumply/pure-events";
  license = stdenv.lib.licenses.bsd3;
}
