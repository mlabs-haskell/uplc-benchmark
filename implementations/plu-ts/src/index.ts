#!/bin/env node

import { compile } from "@harmoniclabs/plu-ts"
import { mkdir, writeFile } from "fs/promises"
import { lpMintingPolicy } from "./validators/lp_minting_policy"
import { nftMintingPolicyValidator } from "./validators/nft_minting_policy"
import { nftMarketplaceValidator } from "./validators/nft_marketplace"
import { poolValidator } from "./validators/pool_validator"


void async function main() {
    await mkdir("./out", { recursive: true })
    await Promise.all([
        writeFile("./out/lp-minting-policy.bin", compile( lpMintingPolicy ) ),
        writeFile("./out/nft-minting-policy.bin", compile( nftMintingPolicyValidator ) ),
        writeFile("./out/nft-marketplace-validator.bin", compile( nftMarketplaceValidator ) ),
        writeFile("./out/pool-validator.bin", compile( poolValidator ) ),
    ])
}()