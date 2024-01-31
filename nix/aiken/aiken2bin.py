#!/usr/bin/env python3

import json
import os

prefix = os.environ.get("out") or "./"

with open("plutus.json", "r") as f:
    validators = json.load(f)["validators"]
    for validator in validators:
        hex_code = bytes.fromhex(validator["compiledCode"])
        name = validator["title"].split(".")[0].replace("_", "-")
        with open(f"{prefix}/{name}.bin", "wb") as w:
            w.write(hex_code)
