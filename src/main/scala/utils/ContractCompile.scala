package utils

import org.ergoplatform.appkit._
import scorex.crypto.hash

class ContractCompile(ctx: BlockchainContext) {

  def compileDummyContract(
      contract: String = "sigmaProp(true)"
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder.empty(),
      contract
    )
  }

  def compileProxyContract(
      contract: String,
      height: Long,
      spender: Address
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item("$height", height)
        .item("$spender", spender.getPublicKey)
        .build(),
      contract
    )
  }
}
