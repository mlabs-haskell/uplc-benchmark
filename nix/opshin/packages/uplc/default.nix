{ lib
, fetchFromGitHub
, python3
, frozenlist2
, python-secp256k1-cardano
}:

python3.pkgs.buildPythonPackage rec {
  pname = "uplc";
  version = "0.6.9";

  format = "pyproject";

  src = fetchFromGitHub {
    owner = "OpShin";
    repo = "uplc";
    rev = version;
    hash = "sha256-djJMNXijMVzMVzw8NZSe3YFRGyAPqdvr0P374Za5XkU=";
  };

  propagatedBuildInputs = with python3.pkgs; [
    setuptools
    poetry-core
    frozendict
    cbor2
    frozenlist2
    rply
    pycardano
    python-secp256k1-cardano
  ];

  pythonImportsCheck = [ "uplc" ];

  meta = with lib; {
    description = "A simple pythonic programming language for Smart Contracts on Cardano";
    homepage = "https://opshin.dev";
    license = licenses.mit;
    maintainers = with maintainers; [ t4ccer ];
    mainProgram = "opshin";
  };
}
