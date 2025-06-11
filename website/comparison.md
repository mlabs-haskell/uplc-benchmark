# Comparison

While trying to implement all validators as closely to each other as possible there are some differences that impact measured script size and execution units.
- Plutarch while decoding Data-encoded types is checking for invariants such as hashes lengths, map key ordering etc. which leads to higher start up cost visible in NFT Marketplace validator.
- Opshin does expose option to compile without tracing so that is included in the final size. All implementations use traces in their source code but remaining allow to strip it with a compilation flag.
- Note that opshin validators are not benchmarked currently as they do not support PlutusV3 yet. They will be restored when they will support it. We have decided to migrate to V3 as Aiken cannot target older Plutus versions.

## Versions

<!-- versions.md -->

## Script size (bytes)

Script sizes are compared by compiling each script to CBOR and taking its binary size in bytes. Validators are compiled without traces if language supports it (Opshin does not) and are not passed through any external UPLC optimizer.

![size plot](./script_size.png)


<!-- script_size.md -->


[Raw script size data](./script_size.csv)

## Execution Units

Execution units are abstract units of CEK machine. Despite being called CPU and Memory they *DO NOT* mean CPU cycles or allocated bytes, these are abstract, deterministic units of the virtual machine that executes UPLC.

### LP Policy - mint one

![execution_units plot](./budget_lp_mint_one_nft.png)


<!-- budget_lp_mint_one_nft.md -->


[Raw execution units data](./budget_lp_mint_one_nft.csv)

### LP Policy - mint two

![execution_units plot](./budget_lp_mint_two_nfts.png)


<!-- budget_lp_mint_two_nfts.md -->


[Raw execution units data](./budget_lp_mint_two_nfts.csv)

### Pool NFT Policy - mint

![execution_units plot](./budget_nft_mint.png)


<!-- budget_nft_mint.md -->


[Raw execution units data](./budget_nft_mint.csv)

### Pool Validator - swap A for B

![execution_units plot](./budget_swap_a_for_b.png)


<!-- budget_swap_a_for_b.md -->


[Raw execution units data](./budget_swap_a_for_b.csv)

### Pool Validator - swap B for A

![execution_units plot](./budget_swap_b_for_a.png)


<!-- budget_swap_b_for_a.md -->


[Raw execution units data](./budget_swap_b_for_a.csv)

### Pool Validator - deposit

![execution_units plot](./budget_deposit.png)


<!-- budget_deposit.md -->


[Raw execution units data](./budget_deposit.csv)

### Pool Validator - withdraw

![execution_units plot](./budget_withdraw.png)


<!-- budget_withdraw.md -->


[Raw execution units data](./budget_withdraw.csv)

### NFT Marketplace - buy one

![execution_units plot](./budget_buy_one.png)


<!-- budget_buy_one.md -->


[Raw execution units data](./budget_buy_one.csv)

### NFT Marketplace - cancel one

![execution_units plot](./budget_cancel_one.png)


<!-- budget_cancel_one.md -->


[Raw execution units data](./budget_cancel_one.csv)
