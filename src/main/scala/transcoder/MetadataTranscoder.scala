package transcoder

import com.google.common.primitives.Longs
import org.ergoplatform.appkit.scalaapi.ErgoValueBuilder
import org.ergoplatform.appkit.{Address, ErgoValue, NetworkType}
import scorex.crypto.hash
import sigmastate.eval.Colls
import special.collection.Coll

import java.nio.charset.StandardCharsets
import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable

class MetadataTranscoder {

  class Encoder {

    def encodeRoyalty(
        royaltyMap: mutable.LinkedHashMap[Address, Int]
    ): ErgoValue[Coll[(Coll[java.lang.Byte], Integer)]] = {

      val royaltyArrayList = new util.ArrayList[(Coll[Byte], Int)]()

      for (element <- royaltyMap.keySet) {
        val key = Colls.fromArray(element.getErgoAddress.script.bytes)
        val value = royaltyMap(element) * 10
        royaltyArrayList.add((key, value))
      }

      val royalty: Coll[(Coll[Byte], Int)] =
        Colls.fromArray(
          royaltyArrayList.asScala.toArray
        )

      ErgoValueBuilder.buildFor(royalty)
    }

    def encodeMetaData(
        explicit: java.lang.Boolean,
        textualTraitsMap: mutable.LinkedHashMap[String, String],
        levelsMap: mutable.LinkedHashMap[String, (Int, Int)],
        statsMap: mutable.LinkedHashMap[String, (Int, Int)]
    ): ErgoValue[
      (
          java.lang.Boolean,
          (
              Coll[(Coll[java.lang.Byte], Coll[java.lang.Byte])],
              (
                  Coll[(Coll[java.lang.Byte], (Integer, Integer))],
                  Coll[(Coll[java.lang.Byte], (Integer, Integer))]
              )
          )
      )
    ] = {
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

      ErgoValueBuilder.buildFor((explicit.asInstanceOf[Boolean], cleanMeta))
    }

    def encodeCollectionInfo(
        collectionInfo: Array[String]
    ): ErgoValue[Coll[Coll[java.lang.Byte]]] = {
      val encodedCollectionInfoArrayList = new util.ArrayList[Coll[Byte]]()
      for (element <- collectionInfo) {
        encodedCollectionInfoArrayList.add(
          Colls.fromArray(element.getBytes(StandardCharsets.UTF_8))
        )
      }

      ErgoValueBuilder.buildFor(
        Colls.fromArray(encodedCollectionInfoArrayList.asScala.toArray)
      )
    }

    def encodeSocialMedaInfo(
        socialMediaMap: mutable.LinkedHashMap[String, String]
    ): ErgoValue[Coll[(Coll[java.lang.Byte], Coll[java.lang.Byte])]] = {

      val encodedSocialsArrayList =
        new util.ArrayList[(Coll[Byte], Coll[Byte])]()

      for (element <- socialMediaMap.keySet) {
        val key = Colls.fromArray(element.getBytes(StandardCharsets.UTF_8))
        val value = Colls.fromArray(
          socialMediaMap(element).getBytes(StandardCharsets.UTF_8)
        )
        encodedSocialsArrayList.add((key, value))
      }

      ErgoValueBuilder.buildFor(
        Colls.fromArray(encodedSocialsArrayList.asScala.toArray)
      )

    }

    def getEmptyAdditionalInfo
        : ErgoValue[Coll[(Coll[java.lang.Byte], Coll[java.lang.Byte])]] = {
      val emptyList =
        new util.ArrayList[(Coll[Byte], Coll[Byte])]()
      ErgoValueBuilder.buildFor(
        Colls.fromArray(emptyList.asScala.toArray)
      )
    }

  }

  class Decoder {

    def decodeRoyalty(
        hexRoyalty: String,
        networkType: NetworkType
    ): mutable.LinkedHashMap[Address, Int] = {

      val royalty =
        ErgoValue.fromHex(hexRoyalty).asInstanceOf[Coll[(Coll[Byte], Int)]]
      val royaltyMap: mutable.LinkedHashMap[Address, Int] =
        mutable.LinkedHashMap()

      for (element <- royalty.toArray) {
        val address = element._1
        val amount = element._2

        royaltyMap += (Address.fromPropositionBytes(
          networkType,
          address.toArray
        ) -> amount)

      }

      royaltyMap
    }

    def hashRoyalty(hexRoyalty: String): Array[Byte] = {
      val royalty =
        ErgoValue
          .fromHex(hexRoyalty)
          .getValue
          .asInstanceOf[Coll[(Coll[Byte], Int)]]

      var royaltyBytes: Array[Byte] =
        Longs.toByteArray(0L)

      for (element <- royalty.toArray) {
        val address = element._1
        val amount = element._2

        royaltyBytes =
          royaltyBytes ++ address.toArray ++ Longs.toByteArray(amount.toLong)

      }

      hash.Blake2b256(royaltyBytes)
    }

    def decodeMetadata(
        hexMetaData: String
    ): Array[_] = {
      val metadata = ErgoValue
        .fromHex(hexMetaData)
        .asInstanceOf[ErgoValue[
          (
              java.lang.Boolean,
              (
                  Coll[(Coll[java.lang.Byte], Coll[java.lang.Byte])],
                  (
                      Coll[(Coll[java.lang.Byte], (Integer, Integer))],
                      Coll[(Coll[java.lang.Byte], (Integer, Integer))]
                  )
              )
          )
        ]]

      val explicit: java.lang.Boolean = metadata.getValue._1
      val traits = metadata.getValue._2._1
      val levels = metadata.getValue._2._2._1
      val stats = metadata.getValue._2._2._2

      val textualTraitsMap: mutable.LinkedHashMap[String, String] =
        mutable.LinkedHashMap()
      val levelsMap: mutable.LinkedHashMap[String, (Int, Int)] =
        mutable.LinkedHashMap()
      val statsMap: mutable.LinkedHashMap[String, (Int, Int)] =
        mutable.LinkedHashMap()

      for (element <- traits.toArray) {
        val key = element._1
        val value = element._2
        textualTraitsMap += (new String(
          key.map(_.toByte).toArray,
          StandardCharsets.UTF_8
        ) -> new String(value.map(_.toByte).toArray, StandardCharsets.UTF_8))
      }

      for (element <- levels.toArray) {
        val key = element._1
        val value = element._2

        levelsMap += (new String(
          key.map(_.toByte).toArray,
          StandardCharsets.UTF_8
        ) -> (value._1, value._2))
      }

      for (element <- stats.toArray) {
        val key = element._1
        val value = element._2

        statsMap += (new String(
          key.map(_.toByte).toArray,
          StandardCharsets.UTF_8
        ) -> (value._1, value._2))
      }

      Array(explicit, textualTraitsMap, levelsMap, statsMap)
    }

    def decodeMetadata(
        metadata: (
            java.lang.Boolean,
            (
                Coll[(Coll[java.lang.Byte], Coll[java.lang.Byte])],
                (
                    Coll[(Coll[java.lang.Byte], (Integer, Integer))],
                    Coll[(Coll[java.lang.Byte], (Integer, Integer))]
                )
            )
        )
    ): Array[_] = {

      val explicit: java.lang.Boolean = metadata._1
      val traits = metadata._2._1
      val levels = metadata._2._2._1
      val stats = metadata._2._2._2

      val textualTraitsMap: mutable.LinkedHashMap[String, String] =
        mutable.LinkedHashMap()
      val levelsMap: mutable.LinkedHashMap[String, (Int, Int)] =
        mutable.LinkedHashMap()
      val statsMap: mutable.LinkedHashMap[String, (Int, Int)] =
        mutable.LinkedHashMap()

      for (element <- traits.toArray) {
        val key = element._1
        val value = element._2
        textualTraitsMap += (new String(
          key.map(_.toByte).toArray,
          StandardCharsets.UTF_8
        ) -> new String(value.map(_.toByte).toArray, StandardCharsets.UTF_8))
      }

      for (element <- levels.toArray) {
        val key = element._1
        val value = element._2

        levelsMap += (new String(
          key.map(_.toByte).toArray,
          StandardCharsets.UTF_8
        ) -> (value._1, value._2))
      }

      for (element <- stats.toArray) {
        val key = element._1
        val value = element._2

        statsMap += (new String(
          key.map(_.toByte).toArray,
          StandardCharsets.UTF_8
        ) -> (value._1, value._2))
      }

      Array(explicit, textualTraitsMap, levelsMap, statsMap)
    }
    def decodeCollectionInfo(hexInfo: String): Array[String] = {
      val info = ErgoValue
        .fromHex(hexInfo)
        .asInstanceOf[ErgoValue[Coll[Coll[java.lang.Byte]]]]
        .getValue

      val collectionInfoArrayList = new util.ArrayList[String]()

      for (element <- info.toArray) {
        val decoded = element.map(_.toByte).toArray
        collectionInfoArrayList.add(new String(decoded, StandardCharsets.UTF_8))
      }
      collectionInfoArrayList.asScala.toArray
    }

    def decodeSocialMedaInfo(
        hexInfo: String
    ): mutable.LinkedHashMap[String, String] = {

      val info = ErgoValue
        .fromHex(hexInfo)
        .asInstanceOf[ErgoValue[
          Coll[(Coll[java.lang.Byte], Coll[java.lang.Byte])]
        ]]
        .getValue

      val socialMediaMap: mutable.LinkedHashMap[String, String] =
        mutable.LinkedHashMap()

      for (element <- info.toArray) {
        val key = element._1
        val value = element._2

        socialMediaMap += (new String(
          key.map(_.toByte).toArray,
          StandardCharsets.UTF_8
        ) -> new String(value.map(_.toByte).toArray, StandardCharsets.UTF_8))
      }

      socialMediaMap
    }

  }
}
