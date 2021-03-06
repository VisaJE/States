package states

import util.control.Breaks._
import scala.math._

abstract class Työ(val kulutus: Vector[Tuote] = Vector(), val tuotto: Vector[Tuote] = Vector(), val koko: Int = 0) {

  
  def tyyppiVertaus(a: Työ): Boolean
 
  
  override def toString: String
  
  
  val ahkeruusEksponentti = Asetus.as("ahkeruuseksponentti")
  def toimi(ahkeruus: Double, kassa: Kassa, määrä: Int): Int = {
    val indeksi = (määrä*ahkeruus).toInt
    breakable  {
      for (i <- 1 to indeksi) {
        if (kassa.kuluta(kulutus)) {
          kassa.lisää(tuotto)   
        }
        else break
      }
    }
  -(määrä*pow(ahkeruus,ahkeruusEksponentti)).toInt
  }
  
  def toimi(ahkeruus: Double, kassa: Kassa): Int = toimi(ahkeruus, kassa, koko)
}


/* Alaluokat. Saman alaluokan edustajilla on syytä olla sama kulutus sekä tuotto. 
 * Työryhmä ei muuten välttämättä jaa työtä tasaisesti.
 */

class Viljely(koko: Int, teho: Int) extends Työ(tuotto = Vector(new Vilja(teho)), koko = koko) {
  
    def tyyppiVertaus(a: Työ) = {
    a match {
      case a: Viljely => true
      case _ => false
    }
  }
  override def toString = " Viljely"  
    
}


class Nollatyö(koko: Int) extends Työ(koko = koko) {

  val tyytyväisyysKerroin = Asetus.asInt("tknollatyö")
  
  def tyyppiVertaus(a: Työ) = false
  // Palauttaa negatiivista tyytyväisyyttä.
  override def toimi(ahkeruus: Double, kassa: Kassa) = {
      tyytyväisyysKerroin*koko
    }
  override def toimi(ahkeruus: Double, kassa: Kassa, määrä: Int) = {
     toimi(ahkeruus, kassa)
  }
  override def toString = " Turhanpanttista lomailua."
}


class Kaivostyö(koko: Int, teho: Int) extends Työ(tuotto = Vector(new Rauta(teho)), koko = koko) {
  
  def tyyppiVertaus(a: Työ) = {
    a match {
      case a: Kaivostyö => true
      case _ => false
    }
  }
    override def toString = " Kaivostyöt"
}


class Tehtailu(koko: Int, teho: Int, raudanTarve: Int) extends Työ(tuotto = Vector(new Työkalut(teho)),
    kulutus = Vector(new Rauta(raudanTarve)), koko = koko) {
  
  
  def tyyppiVertaus(a: Työ) = {
    a match {
      case a: Tehtailu => true
      case _ => false
    }
  }
  
  
  override def toString = " Raudan jalostus"
}