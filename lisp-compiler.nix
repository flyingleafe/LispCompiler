{ mkDerivation, attoparsec, base, base-unicode-symbols, bytestring
, comonad, containers, directory, lens, mtl, process, stdenv
, temporary, text
}:
mkDerivation {
  pname = "fv-lisp-compiler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base base-unicode-symbols bytestring comonad containers
    directory lens mtl process temporary text
  ];
  description = "Simple lisp compiler";
  license = "GPL";
}
