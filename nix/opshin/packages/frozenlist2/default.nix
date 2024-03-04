{ lib
, fetchFromGitHub
, python3
}:

python3.pkgs.buildPythonPackage rec {
  pname = "frozenlist2";
  version = "1.0.0";

  format = "pyproject";

  src = fetchFromGitHub {
    owner = "rohanpm";
    repo = "frozenlist2";
    rev = "v${version}";
    hash = "sha256-fF0oFZ2q1wRH7IKBlCjm3Za4xtEMSHyEaGL09rHgtTY=";
  };

  propagatedBuildInputs = with python3.pkgs; [
    setuptools
  ];

  pythonImportsCheck = [ "frozenlist2" ];

  meta = with lib; {
    description = "An immutable list for Python";
    homepage = "https://github.com/rohanpm/frozenlist2";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ t4ccer ];
  };
}
