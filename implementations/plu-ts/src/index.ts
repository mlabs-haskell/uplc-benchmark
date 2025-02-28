#!/bin/env node

import { Cbor, CborBytes } from "@harmoniclabs/cbor";
import { compile, Term, PType } from "@harmoniclabs/plu-ts"
import { mkdir, writeFile } from "fs/promises"
import { lpMintingPolicy } from "./validators/lp_minting_policy"
import { nftMintingPolicyValidator } from "./validators/nft_minting_policy"
import { nftMarketplaceValidator } from "./validators/nft_marketplace"
import { poolValidator } from "./validators/pool_validator"

function compileScript(term: Term<PType>) {
    return Cbor.encode(new CborBytes(compile(term))).toBuffer();
}

void async function main() {
    await mkdir("./out", { recursive: true })
    await Promise.all([
        writeFile("./out/lp-minting-policy.bin", compileScript( lpMintingPolicy ) ),
        writeFile("./out/nft-minting-policy.bin", compileScript( nftMintingPolicyValidator ) ),
        writeFile("./out/nft-marketplace-validator.bin", compileScript( nftMarketplaceValidator ) ),
        writeFile("./out/pool-validator.bin", compileScript( poolValidator ) ),
    ])
}()
