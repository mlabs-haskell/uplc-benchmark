# Comparison

## Script size (bytes)

Script sizes are compared by compiling each script to CBOR and taking its binary size in bytes. Validators are compiled without traces if language supports it (Opshin does not) and are not passed through any external UPLC optimizer.

![size plot](./script_size.png)


<!-- script_size.md -->


[Raw script size data](./script_size.csv)

## Execution Units

### NFT Marketplace - buy one

![execution_units plot](./budget_buy_one.png)


<!-- budget_buy_one.md -->


[Raw execution units data](./budget_buy_one.csv)

### NFT Marketplace - cancel one

![execution_units plot](./budget_cancel_one.png)


<!-- budget_cancel_one.md -->


[Raw execution units data](./budget_cancel_one.csv)
