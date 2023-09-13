package utils

import org.apache.http._
import org.apache.http.client.methods.{HttpPatch, HttpPost}
import org.apache.http.impl.client.HttpClients
import org.apache.http.message.BasicNameValuePair
import com.google.gson.{Gson, GsonBuilder}
import configs.serviceOwnerConf
import org.apache.http.entity.{ContentType, StringEntity}
import org.apache.http.util.EntityUtils

import java.util

case class MintEntry(
    issuer: String,
    creation_tx_hash: String,
    mint_tx_hash: String
)

case class MintUpdate(
    mint_tx_hash: String
)

object DatabaseAPI {

  private val serviceFilePath = "serviceOwner.json"
  private lazy val serviceConf = serviceOwnerConf.read(serviceFilePath)

  private val apiKey =
    ""
  private val tableEndpointURI =
    ""

  def createMintEntry(
      issuer_box_id: String,
      creation_tx_hash: String,
      mint_tx_hash: String
  ): Int = {
    val dbEntry = new MintEntry(
      issuer_box_id,
      creation_tx_hash,
      mint_tx_hash
    )
    val entryAsJson: String = new Gson().toJson(dbEntry)
    val requestEntity = new StringEntity(
      entryAsJson,
      ContentType.APPLICATION_JSON
    )

    val post = new HttpPost(
      tableEndpointURI
    )
    val nameValuePairs = new util.ArrayList[NameValuePair]()
    nameValuePairs.add(new BasicNameValuePair("JSON", entryAsJson))
    post.setEntity(requestEntity)

    post.setHeader("apikey", apiKey)
    post.setHeader(HttpHeaders.AUTHORIZATION, s"Bearer ${apiKey}")
    post.setHeader(HttpHeaders.CONTENT_TYPE, "application/json")

    // send the post request
    val client = HttpClients.custom().build()
    val response = client.execute(post)

    response.getStatusLine.getStatusCode

  }

  def updateMintEntry(
      issuer_box_id: String,
      mint_tx_hash: String
  ): Int = {

    // constructing the patch URL
    val patchUrl = s"${tableEndpointURI}?issuer=eq.$issuer_box_id"

    val dbEntry = new MintUpdate(
      mint_tx_hash
    )

    val entryAsJson: String = new Gson().toJson(dbEntry)

    val entity = new StringEntity(
      entryAsJson,
      ContentType.APPLICATION_JSON
    )

    val patch = new HttpPatch(patchUrl)
    patch.setEntity(entity)
    patch.setHeader("apikey", apiKey)
    patch.setHeader(HttpHeaders.AUTHORIZATION, s"Bearer ${apiKey}")
    patch.setHeader(
      HttpHeaders.CONTENT_TYPE,
      ContentType.APPLICATION_JSON.getMimeType
    )
    patch.setHeader("Prefer", "return=minimal")

    val client = HttpClients.custom().build()
    val response = client.execute(patch)

    response.getStatusLine.getStatusCode
  }
}
