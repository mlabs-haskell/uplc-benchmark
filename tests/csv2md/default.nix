{ lib
, buildPythonPackage
, fetchFromGitHub
, setuptools
}:

buildPythonPackage rec {
  pname = "csv2md";
  version = "1.3.0";

  src = fetchFromGitHub {
    owner = "lzakharov";
    repo = "csv2md";
    rev = "v${version}";
    hash = "sha256-INwZRbyJmyDd5SV7PBdqUc/3FO91upTgaxBfP/Giaw4=";
  };

  propagatedBuildInputs = [
    setuptools
  ];

  meta = with lib; {
    description = "Command line tool for converting CSV files into Markdown tables";
    homepage = "https://github.com/lzakharov/csv2md";
    license = licenses.mit;
    maintainers = with maintainers; [ t4ccer ];
    mainProgram = "csv2md";
  };
}
