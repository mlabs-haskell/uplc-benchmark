{ lib
, fetchFromGitHub
, python3
}:

python3.pkgs.buildPythonPackage {
  pname = "graphlib-backport";
  version = "1.0.3";

  format = "pyproject";

  src = fetchFromGitHub {
    owner = "mariushelf";
    repo = "graphlib_backport";
    rev = "113473ecf2b5b296cadcd1d2439908dca8affc01"; # No tags in repo
    hash = "sha256-3gi9Xggrpo5DL6VPMUjbmV6wiDuDF2JJmWaT2ljrn2M=";
  };

  postPatch = ''
    substituteInPlace pyproject.toml        \
      --replace 'poetry>=1.0' 'poetry-core' \
      --replace 'poetry.masonry.api' 'poetry.core.masonry.api'
  '';

  propagatedBuildInputs = with python3.pkgs; [
    setuptools
    poetry-core
  ];

  pythonImportsCheck = [ "graphlib" ];

  meta = with lib; {
    description = "Backport of the Python 3.9 graphlib module for Python 3.6+";
    homepage = "https://github.com/mariushelf/graphlib_backport";
    license = licenses.psfl;
    maintainers = with maintainers; [ t4ccer ];
  };
}
