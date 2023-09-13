package transcoder

import configs.Collection
import org.ergoplatform.appkit.{Address, ErgoValue}
import org.ergoplatform.appkit.scalaapi.ErgoValueBuilder
import sigmastate.eval.Colls
import special.collection.Coll

import java.nio.charset.StandardCharsets
import java.util
import java.util.{Map => JMap}
import scala.collection.JavaConverters._
import scala.collection.mutable

class encoderHelper(collectionFile: Collection) {
  private val metadataTranscoder = new MetadataTranscoder
  private val encoder = new metadataTranscoder.Encoder

  private def convertToMutableMap(
      jmap: JMap[String, String]
  ): mutable.LinkedHashMap[String, String] = {
    mutable.LinkedHashMap(jmap.asScala.toSeq: _*)
  }

  def encodeCollectionInfo: String = {

    val collectionInfo = Array(
      collectionFile.collectionInfo.collectionLogoURL,
      collectionFile.collectionInfo.collectionFeaturedImageURL,
      collectionFile.collectionInfo.collectionBannerImageURL,
      collectionFile.collectionInfo.collectionCategory
    )

    encoder.encodeCollectionInfo(collectionInfo).toHex
  }

  def encodeSocials: String = {

    encoder
      .encodeSocialMedaInfo(
        convertToMutableMap(collectionFile.socialMedia)
      )
      .toHex
  }

  def getEmptyAdditionalInfo: String = {
    "0c3c0e0e010000"
  }

  def encodeMetadata(
      explicit: java.lang.Boolean,
      textualTraitsMap: mutable.LinkedHashMap[String, String],
      levelsMap: mutable.LinkedHashMap[String, (Int, Int)],
      statsMap: mutable.LinkedHashMap[String, (Int, Int)]
  ): String = {
    val textualTraitsArrayList =
      new util.ArrayList[(Coll[Byte], Coll[Byte])]()
    val levelsArrayList = new util.ArrayList[(Coll[Byte], (Int, Int))]()
    val statsArrayList = new util.ArrayList[(Coll[Byte], (Int, Int))]()

    for (element <- textualTraitsMap.keySet) {
      val key = Colls.fromArray(element.getBytes(StandardCharsets.UTF_8))
      val value = Colls.fromArray(
        textualTraitsMap(element).getBytes(StandardCharsets.UTF_8)
      )
      textualTraitsArrayList.add((key, value))
    }

    for (element <- levelsMap.keySet) {
      val key = Colls.fromArray(element.getBytes(StandardCharsets.UTF_8))
      val value = levelsMap(element)
      levelsArrayList.add((key, value))
    }

    for (element <- statsMap.keySet) {
      val key = Colls.fromArray(element.getBytes(StandardCharsets.UTF_8))
      val value = statsMap(element)
      statsArrayList.add((key, value))
    }

    val cleanMeta = (
      Colls.fromArray(textualTraitsArrayList.asScala.toArray),
      (
        Colls.fromArray(levelsArrayList.asScala.toArray),
        Colls.fromArray(statsArrayList.asScala.toArray)
      )
    )

    ErgoValueBuilder.buildFor(cleanMeta).toHex
  }

  def encodeRoyalty: String = {
    val royaltyMap: mutable.LinkedHashMap[Address, Int] =
      mutable.LinkedHashMap()
    for (royalty <- collectionFile.royalty) {
      royaltyMap += (Address.create(
        royalty.address
      ) -> royalty.amount.round.toInt)
    }
    encoder.encodeRoyalty(royaltyMap).toHex
  }
}
