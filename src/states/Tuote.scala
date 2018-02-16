package states


// Tuotteen määrä on mutable!
abstract class Tuote(val arvo: Int, private var koko: Int) {

  
  // Määriteltävä alaluokassa
  def tyyppiVertaus(a: Tuote): Boolean
  override def toString: String
  
  
  def lisää(tuote: Tuote) = {
    if (this.tyyppiVertaus(tuote)) {
    this.koko += tuote.määrä
    true
    }
    else {
      false
    }
  }
  
  
  // Ilmoittaa, onko tätä tuotetta enemmän kuin parametrin tuotetta.
  def riittääkö(tuote: Tuote): Boolean = {
    if (!tyyppiVertaus(tuote)) throw VääräTuoteVirhe()
    else {
      tuote.määrä <= this.määrä
    }
  }
  
  
  def kuluta(tuote: Tuote): Boolean = {
      val saatavuus = riittääkö(tuote)
      if (saatavuus) {
        this.koko -= tuote.määrä
        true
      }
      else false
  }
  
  def hinta = määrä*arvo
  
  def määrä: Int = this.koko
  

}



// Alaluokat

class Raha(m: Int = 0) extends Tuote(1, m) {
  
  
  def tyyppiVertaus(a: Tuote) = {
    a match {
      case a: Raha => true
      case _ => false
    }
  }
  
  override def toString = {
    määrä + "€"
  }
  
}

// Rahan käsittelyn helpottamiseksi.
object Raha {
  def apply(määrä: Int) = new Raha(määrä)
}


class Vilja(m: Int = 0) extends Tuote(2, m) {
  
  
  def tyyppiVertaus(a: Tuote) = {
    a match {
      case a: Vilja => true
      case _ => false
    }
  }
  
  override def toString = {
    "Viljaa " + määrä + " leiviskää"
  }
  
}







