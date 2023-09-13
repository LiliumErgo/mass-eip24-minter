package app

import configs.{collectionParser, masterMeta, serviceOwnerConf}
import contracts.MassMinterContracts
import execute.Client
import org.ergoplatform.Input
import org.ergoplatform.appkit.{Address, InputBox, SignedTransaction}
import transcoder.encoderHelper
import utils.{
  BoxAPI,
  ContractCompile,
  DatabaseAPI,
  TransactionHelper,
  explorerApi
}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._

object Minter extends App {

//  val arglist = args.toList
//  type OptionMap = Map[Symbol, String]
//
//  @tailrec
//  private def nextOption(map: OptionMap, list: List[String]): OptionMap = {
//    list match {
//      case Nil => map
//      case "--conf" :: value :: tail =>
//        nextOption(map ++ Map('conf -> value.toString), tail)
//      case "--dryrun" :: tail =>
//        nextOption(map ++ Map('dryrun -> "true"), tail)
//      case option :: tail =>
//        println("Unknown option " + option)
//        sys.exit(1)
//    }
//  }
//
//  private val options = nextOption(Map(), arglist)
//
//  private val confFilePath = options.get('conf)
//  private val dryRun = options.contains('dryrun)
//
//  confFilePath match {
//    case Some(path) => // Continue processing
//    case None =>
//      println("Configuration file path not provided.")
//      sys.exit(1)
//  }

//  private val serviceOwnerConfigFilePath: String = confFilePath.get
  private val serviceOwnerConfigFilePath: String = "serviceOwner.json"
  private val serviceOwnerConfig =
    serviceOwnerConf.read(serviceOwnerConfigFilePath)

  private val collectionConfig =
    try {
      collectionParser.read("collection.json")
    } catch {
      case e: Exception =>
        println(e); sys.exit(1)
    }

  private val metadataConfig =
    try {
      masterMeta.read("final_meta_1.json")
    } catch {
      case e: Exception =>
        println("could not read collection.json"); sys.exit(1)
    }

  private val encoder = new encoderHelper(collectionConfig)

  private val walletMnemonic = serviceOwnerConfig.txOperatorMnemonic
  private val walletMnemonicPw = serviceOwnerConfig.txOperatorMnemonicPw

  private val client: Client = new Client()
  client.setClient
  private val ctx = client.getContext

  val explorerClient: explorerApi = new explorerApi()
  private val boxAPIObj =
    new BoxAPI(serviceOwnerConfig.apiUrl, serviceOwnerConfig.nodeUrl)

  private val txHelper =
    new TransactionHelper(
      this.ctx,
      walletMnemonic,
      walletMnemonicPw
    )

  val compiler = new ContractCompile(ctx)

  private val proxyScript = MassMinterContracts.ProxyContract.contractScript
  private val proxyContract =
    compiler.compileProxyContract(proxyScript, 100L, txHelper.senderAddress)

  private val minerFee = 1000000L
  private val issuanceMinValue = 3000000L
  private val minBoxValue = 1000000L
  private val mintExp = collectionConfig.mintingExpiry
  private val minerErgoTree =
    "1005040004000e36100204a00b08cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ea02d192a39a8cc7a701730073011001020402d19683030193a38cc7b2a57300000193c2b2a57301007473027303830108cdeeac93b1a57304"

  private val recipient =
    Address.create("9iL4mMos5DuC7cBj6zAZr5yyqMjZUESH748kxG4Gdsr3ZR3TeBR")
//  println(proxyContract.toAddress.toString)

  private val writeToDatabase = false

  val boxes =
    boxAPIObj
      .getUnspentBoxesFromApi(
        txHelper.senderAddress.toString,
        selectAll = true
      )
      .items
      .map(boxAPIObj.convertJsonBoxToInputBox)

  val encodedCollectionInfo = encoder.encodeCollectionInfo
  val encodedSocials = encoder.encodeSocials
  val encodedRoyalty = encoder.encodeRoyalty

  val collectionIssuerTx = txHelper.createCollectionIssuerTransaction(
    boxes,
    (boxes.map(_.getValue).sum - minerFee),
    1,
    encodedCollectionInfo,
    encodedSocials,
    mintExp,
    encoder.getEmptyAdditionalInfo
  )

  val collectionIssuerTxHash = txHelper.sendTx(collectionIssuerTx)

  println("collection issuer tx: " + collectionIssuerTxHash)

  val collectionIssuanceTx = txHelper.createCollectionTokenTransaction(
    Seq(collectionIssuerTx.getOutputsToSpend.get(0)),
    collectionIssuerTx.getOutputsToSpend.get(0).getValue - minerFee,
    collectionConfig.collectionInfo.collectionName,
    collectionConfig.collectionInfo.collectionDescription,
    metadataConfig.length
  )

  val collectionIssuanceTxHash = txHelper.sendTx(collectionIssuanceTx)
  println("collection issuance tx: " + collectionIssuanceTxHash)

  val eip24issuerCreationTx = txHelper.createEip24IssuanceTransaction(
    Seq(collectionIssuanceTx.getOutputsToSpend.get(0)),
    issuanceMinValue,
    minerFee,
    metadataConfig,
    encoder,
    encodedRoyalty,
    encoder.getEmptyAdditionalInfo,
    collectionIssuanceTx.getOutputsToSpend.get(0).getTokens.get(0),
    100
  )

  eip24issuerCreationTx.foreach(tx => {
    var success = false
    while (!success) {
      try {
        val hash = txHelper.sendTx(tx)
        println("eip24 issuer tx: " + hash)
        Thread.sleep(500)
        success = true
      } catch {
        case e: Exception =>
          println(s"Exception while sending tx: ${e.getMessage}")
          Thread.sleep(10000) // sleep for 10 seconds
      }
    }
  })

  val issuanceInputs = new ListBuffer[InputBox]()

  for (issuances <- eip24issuerCreationTx.tail) {
    for (output <- issuances.getOutputsToSpend.asScala) {
      if (
        output.getErgoTree.bytesHex != minerErgoTree && output.getTokens
          .size() > 0
      ) {
        issuanceInputs.append(output)

        if (writeToDatabase) {

          var success = false
          while (!success) {
            try {
              val resp = DatabaseAPI.createMintEntry(
                output.getId.toString(),
                issuances.getId,
                "empty"
              )
              if (resp >= 200 && resp < 300) {
                success = true
              }
            } catch {
              case e: Exception =>
                println(
                  s"Exception while writing issuer id to supabase: ${e.getMessage}"
                )
                Thread.sleep(1000) // sleep for 1 second
            }
          }
        }
      }
    }
  }

  issuanceInputs.zipWithIndex.par.foreach { case (issuer, index) =>
    var success = false
    while (!success) {
      try {
        val mintTx = txHelper.mintEip24Nft(
          issuer,
          metadataConfig(index),
          minBoxValue,
          recipient
        )
        val txHash = txHelper.sendTx(mintTx)
        println(s"mint tx #${index + 1}: " + txHash)

        if (writeToDatabase) {
          var supaBaseSuccess = false
          while (!supaBaseSuccess) {
            try {
              val resp = DatabaseAPI.updateMintEntry(
                issuer.getId.toString(),
                mintTx.getId.toString
              )
              if (resp >= 200 && resp < 300) {
                supaBaseSuccess = true
              }
            } catch {
              case e: Exception =>
                println(
                  s"Exception while writing mint to supabase: ${e.getMessage}"
                )
                Thread.sleep(10000) // sleep for 1 second
            }
          }
        }

        success = true

      } catch {
        case e: Exception =>
          println(s"Exception while minting tx #${index + 1}: ${e.getMessage}")
          Thread.sleep(60000) // sleep for 60 seconds
      }
    }
  }

}
