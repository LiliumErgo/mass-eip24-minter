{
    // ===== Contract Description ===== //
    // Name: NFT Buyer Proxy Contract
    // Description: This contract is a proxy contract and ensures funds are used for NFTs or are refunded.
    // Version: 1.0.0
    // Author: mgpai22@github.com

    // ===== Box Registers ===== //
    // None

    // ===== Compile Time Constants ===== //
    // $height: Long
    // $spender: SigmaProp

    // ===== Context Extension Variables ===== //
    // None

    sigmaProp(CONTEXT.HEIGHT >= $height) && $spender

}
