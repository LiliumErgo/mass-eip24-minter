package transcoder

import configs.Collection
import org.ergoplatform.appkit.Address

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
    encoder.getEmptyAdditionalInfo.toHex
  }

  def encodeMetadata(
      explicit: Boolean,
      textualTraitsMap: mutable.LinkedHashMap[String, String],
      levelsMap: mutable.LinkedHashMap[String, (Int, Int)],
      statsMap: mutable.LinkedHashMap[String, (Int, Int)]
  ): String = {
    encoder
      .encodeMetaData(
        explicit,
        textualTraitsMap,
        levelsMap,
        statsMap
      )
      .toHex
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
