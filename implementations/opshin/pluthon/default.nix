{ lib
, fetchFromGitHub
, python3
, uplc
, graphlib-backport
}:

python3.pkgs.buildPythonPackage rec {
  pname = "pluthon";
  version = "0.4.6";

  format = "pyproject";

  src = fetchFromGitHub {
    owner = "OpShin";
    repo = "pluthon";
    rev = version;
    hash = "sha256-ZmBkbglSbBfVhA4yP0tJdwpJiFpJ7vX0A321ldQF0lA=";
  };

  propagatedBuildInputs = with python3.pkgs; [
    setuptools
    uplc
    graphlib-backport
    ordered-set
  ];

  pythonImportsCheck = [ "pluthon" ];

  meta = with lib; {
    description = "Pluto-like programming language for Cardano Smart Contracts in Python";
    homepage = "https://github.com/OpShin/pluthon";
    license = licenses.mit;
    maintainers = with maintainers; [ t4ccer ];
  };
}
