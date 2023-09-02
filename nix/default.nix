{ mkDerivation, aeson, aeson-pretty, base, bytestring, extra, lib
, shake, text
}:
mkDerivation {
  pname = "prebuilder";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring extra shake text
  ];
  license = "unknown";
  mainProgram = "prebuilder";
}
