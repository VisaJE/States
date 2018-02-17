package states

import util.control.Breaks._

abstract class Työ(val kulutus: Vector[Tuote] = Vector(), val tuotto: Vector[Tuote] = Vector(), val koko: Int = 0) {

  
  def tyyppiVertaus(a: Työ): Boolean
 
  
  // Palauttaa ryhmän tyytyväisyyden
  def toimi(ahkeruus: Double, kassa: Kassa, määrä: Int): Int = {
    val indeksi = (määrä*ahkeruus).toInt
    var tyytyväisyys = 0
    for (i <- 1 to indeksi) {
      if (kassa.kuluta(kulutus)) {
        kassa.lisää(tuotto)
        tyytyväisyys -= 1
      }
      else break
    }
    tyytyväisyys
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
    
    
}


class Nollatyö(koko: Int) extends Työ(koko = koko) {

  val tyytyväisyysKerroin = -2
  
  def tyyppiVertaus(a: Työ) = false
  // Palauttaa negatiivista tyytyväisyyttä.
  override def toimi(ahkeruus: Double, kassa: Kassa) = {
      tyytyväisyysKerroin*koko
    }
  override def toimi(ahkeruus: Double, kassa: Kassa, määrä: Int) = {
     toimi(ahkeruus, kassa)
  }
   
}


class Kaivostyö(koko: Int, teho: Int) extends Työ(tuotto = Vector(new Rauta(teho)), koko = koko) {
  
    def tyyppiVertaus(a: Työ) = {
    a match {
      case a: Kaivostyö => true
      case _ => false
    }
  }
}