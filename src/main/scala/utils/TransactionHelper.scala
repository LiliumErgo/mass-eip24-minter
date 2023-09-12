package utils

import configs.Data
import org.bouncycastle.util.encoders.Hex
import org.ergoplatform.appkit.{
  Address,
  BlockchainContext,
  ErgoValue,
  InputBox,
  Mnemonic,
  OutBox,
  SignedTransaction,
  UnsignedTransaction
}
import org.ergoplatform.sdk.{ErgoToken, SecretString}
import transcoder.encoderHelper

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TransactionHelper(
    ctx: BlockchainContext,
    walletMnemonic: String,
    mnemonicPassword: String = ""
) {
  private val mnemonic = Mnemonic.create(
    SecretString.create(walletMnemonic),
    SecretString.create(mnemonicPassword)
  )
  private val txBuilder = this.ctx.newTxBuilder()

  val senderAddress: Address = Address.createEip3Address(
    0,
    ctx.getNetworkType,
    SecretString.create(walletMnemonic),
    SecretString.create(mnemonicPassword),
    false
  )

  private val minAmount = 1000000L

  def buildUnsignedTransaction(
      inputs: Seq[InputBox],
      outputs: Seq[OutBox],
      dataInputs: Seq[InputBox] = Seq.empty,
      tokensToBurn: Seq[ErgoToken] = Seq.empty,
      fee: Long = minAmount
  ): UnsignedTransaction = {
    val builder = this.ctx
      .newTxBuilder()
      .addInputs(inputs: _*)
      .addOutputs(outputs: _*)
      .fee(fee)
      .sendChangeTo(this.senderAddress)

    if (dataInputs.nonEmpty) builder.addDataInputs(dataInputs: _*)
    if (tokensToBurn.nonEmpty) builder.tokensToBurn(tokensToBurn: _*)

    builder.build()
  }

  def signTransaction(
      unsignedTransaction: UnsignedTransaction,
      proverIndex: Int = 0
  ): SignedTransaction = {
    val prover = this.ctx
      .newProverBuilder()
      .withMnemonic(mnemonic, false)
      .withEip3Secret(proverIndex)
      .build()
    prover.sign(unsignedTransaction)
  }
  def sendTx(signedTransaction: SignedTransaction): String = {
    this.ctx.sendTransaction(signedTransaction)
  }

  def createToken(
      receiver: Address,
      amountList: Seq[Long],
      inputBox: Option[Seq[InputBox]] = None,
      sender: Address = this.senderAddress,
      isCollection: Boolean = false,
      name: String,
      description: String,
      tokenAmount: Long,
      tokenDecimals: Int
  ): SignedTransaction = {
    val inBox: Seq[InputBox] = inputBox.getOrElse(
      new InputBoxes(ctx).getInputs(amountList, sender)
    )
    val outBoxObj = new OutBoxes(this.ctx)

    val token = if (isCollection) {
      outBoxObj.collectionTokenHelper(
        inBox.head,
        name,
        description,
        tokenAmount,
        tokenDecimals
      )
    } else {
      outBoxObj.tokenHelper(
        inBox.head,
        name,
        description,
        tokenAmount,
        tokenDecimals
      )
    }

    val outBox =
      outBoxObj.tokenMintOutBox(token, receiver, amount = amountList.head)
    val unsignedTransaction =
      this.buildUnsignedTransaction(inBox, Seq(outBox))

    this.signTransaction(unsignedTransaction)
  }

  def createInputSplitterTransaction(
      inputs: Seq[InputBox],
      outboxes: Seq[Seq[OutBox]],
      minerFee: Long,
      amountOfEqualInputs: Long
  ): SignedTransaction = {
    val totalInputValue = inputs.map(_.getValue).sum - minerFee
    val majorityValue = totalInputValue / amountOfEqualInputs
    val finalBoxValue =
      majorityValue + (totalInputValue - (majorityValue * amountOfEqualInputs))

    val outBoxObj = new OutBoxes(this.ctx)
    val splitterOutBoxList = new ListBuffer[OutBox]

    for ((outboxList, index) <- outboxes.zipWithIndex) {
      val value =
        if (index == outboxes.length - 1) finalBoxValue else majorityValue
      val splitterOutBox = outBoxObj.tokenOutBox(
        Seq(
          new ErgoToken(
            outboxList.head.getTokens.get(0).getId,
            outboxList.length.toLong
          )
        ),
        this.senderAddress,
        value
      )
      splitterOutBoxList.append(splitterOutBox)
    }

    val unsignedTransaction =
      this.buildUnsignedTransaction(inputs, splitterOutBoxList)

    this.signTransaction(unsignedTransaction)
  }

  def createCollectionIssuerTransaction(
      inputs: Seq[InputBox],
      boxValue: Long,
      collectionStandardVersion: Int = 1,
      encodedCollectionInfo: String,
      encodedSocials: String,
      mintExpiry: Long,
      encodedAdditionalInfo: String
  ): SignedTransaction = {
    val outBoxObj = new OutBoxes(this.ctx)
    val outbox = outBoxObj.buildIssuerBox(
      this.senderAddress,
      Array(
        ErgoValue.of(collectionStandardVersion),
        ErgoValue.fromHex(encodedCollectionInfo),
        ErgoValue.fromHex(encodedSocials),
        ErgoValue.of(mintExpiry),
        ErgoValue.fromHex(encodedAdditionalInfo)
      ),
      boxValue
    )
    val unsignedTransaction =
      this.buildUnsignedTransaction(inputs, Seq(outbox))

    this.signTransaction(unsignedTransaction)
  }

  def createCollectionTokenTransaction(
      inputs: Seq[InputBox],
      boxValue: Long,
      collectionName: String,
      collectionDescription: String,
      collectionSize: Long
  ): SignedTransaction = {
    this.createToken(
      this.senderAddress,
      List(boxValue),
      Some(inputs),
      isCollection = true,
      name = collectionName,
      description = collectionDescription,
      tokenAmount = collectionSize,
      tokenDecimals = 0
    )
  }

  def createEip24IssuanceTransaction(
      inputs: Seq[InputBox],
      boxValuePerIssuanceBox: Long,
      minerFee: Long,
      metadata: Array[Data],
      encoder: encoderHelper,
      encodedRoyalty: String,
      encodedEmptyAdditionalInfo: String,
      collectionToken: ErgoToken,
      transactionBoxLimit: Int = 500
  ): Seq[SignedTransaction] = {

    def calculateTransactions(size: Int, transactionSize: Int): Int = {
      val quotient = size / transactionSize
      val remainder = size % transactionSize
      if (remainder > 0) quotient + 1 else quotient
    }

    val amountTransactions =
      calculateTransactions(metadata.length, transactionBoxLimit)
    val totalMinerFee = amountTransactions * minerFee
    val totalBoxValue =
      (boxValuePerIssuanceBox * metadata.length) + totalMinerFee
    val totalInputValue = inputs.map(_.getValue).sum

    if (totalBoxValue > totalInputValue) {
      throw new Error("inputs cannot cover total cost of all issuance boxes")
    }

    val outBoxObj = new OutBoxes(this.ctx)

    val metadataList = new ListBuffer[String]
    val outboxList = new ListBuffer[OutBox]

    for (data <- metadata) {
      val attributesMap = mutable.LinkedHashMap(
        data.attributes.map(a => a.trait_type -> a.value): _*
      )

      val levelsMap = mutable.LinkedHashMap(
        data.levels.map(a => a.trait_type -> (a.value, a.max_value)): _*
      )
      val statsMap = mutable.LinkedHashMap(
        data.stats.map(a => a.trait_type -> (a.value, a.max_value)): _*
      )

      val encodedMetadata: String = encoder.encodeMetadata(
        data.explicit,
        attributesMap,
        levelsMap,
        statsMap
      )
      metadataList.append(encodedMetadata)
    }

    for (metadata <- metadataList) {
      val issuerRegisters: Array[ErgoValue[_]] = Array(
        ErgoValue.of(2),
        ErgoValue.fromHex(encodedRoyalty),
        ErgoValue.fromHex(metadata),
        ErgoValue.of(collectionToken.getId.getBytes),
        ErgoValue.fromHex(encodedEmptyAdditionalInfo)
      )
      val issuerOutBox =
        outBoxObj.buildEIP24IssuerBox(
          this.senderAddress,
          issuerRegisters,
          new ErgoToken(
            collectionToken.getId,
            1L
          ),
          boxValuePerIssuanceBox
        )
      outboxList.append(issuerOutBox)
    }

    val outboxLists = outboxList.grouped(transactionBoxLimit).toList

    val issuerBoxesInputs = this.createInputSplitterTransaction(
      inputs,
      outboxLists,
      minerFee,
      amountTransactions
    )

    val transactionsToSubmit = new ListBuffer[SignedTransaction]

    transactionsToSubmit.append(issuerBoxesInputs)

    for ((boxes, index) <- outboxLists.zipWithIndex) {
      val unsignedTransaction =
        this.buildUnsignedTransaction(
          Seq(
            issuerBoxesInputs.getOutputsToSpend.get(index)
          ),
          boxes,
          fee = minerFee
        )

      transactionsToSubmit.append(this.signTransaction(unsignedTransaction))
    }

    transactionsToSubmit
  }

  def mintEip24Nft(
      input: InputBox,
      metadata: Data,
      boxValue: Long
  ): SignedTransaction = {
    val outBoxObj = new OutBoxes(this.ctx)

    val nft = outBoxObj.pictureNFTHelper(
      input,
      metadata.name,
      metadata.description,
      metadata.image,
      Hex.decode(metadata.imageSHA256)
    )

    val nftOutBox = outBoxObj.NFToutBox(
      nft,
      this.senderAddress,
      boxValue
    )

    val tokenOut = outBoxObj.tokenOutBox(
      Seq(input.getTokens.get(0)),
      this.senderAddress
    )

    val unsignedTransaction =
      this.buildUnsignedTransaction(Seq(input), Seq(nftOutBox, tokenOut))

    this.signTransaction(unsignedTransaction)
  }

}
