package states

import scala.io._
import scala.collection.mutable.Map

object Asetus {
  private val asetukset = Source.fromFile(".asetukset")
  private val löydetyt: Map[String, Double] = Map()
  for (i <- asetukset.getLines()) {
    val osat = i.replace(" ", "").split("=")
    if (osat.size == 2) {
      löydetyt += osat(0) -> osat(1).toDouble
    }
  }
  
  
  // Palauttaa asetuksen double-arvona.
  def as(s: String): Double = {
    try {
      löydetyt(s)
    }
    catch {
      case x: Throwable => throw AsetusVirhe(message = "Virhe haulle: " + s, cause = x)
    }
  }
  
  // Palauttaa asetuksen Int-arvona.
  def asInt(s: String) = as(s).toInt
  
}