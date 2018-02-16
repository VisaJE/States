package states


abstract class Tuote(val arvo: Int, val määrä: Int) {

      
      
  // Määriteltävä alaluokassa
  def tyyppiVertaus(a: Tuote): Boolean
  override def toString: String
  def copy(m: Int): Tuote
  
  
  def lisää(tuote: Tuote): Tuote= {
    if (this.tyyppiVertaus(tuote)) {
    this.copy(määrä + tuote.määrä)
    }
    else {
      this
    }
  }
  
  
  // Ilmoittaa, onko tätä tuotetta enemmän kuin parametrin tuotetta.
  def riittääkö(tuote: Tuote): Boolean = {
    if (!tyyppiVertaus(tuote)) throw VääräTuoteVirhe()
    else {
      tuote.määrä <= this.määrä
    }
  }
  
  
  def kuluta(tuote: Tuote): Tuote = {
      val saatavuus = riittääkö(tuote)
      if (saatavuus) {
       this.copy(määrä - tuote.määrä)
      }
      else this
  }
  
  def hinta = määrä*arvo
  
  

}



// Alaluokat

class Raha(m: Int = 0) extends Tuote(1, m) {
  
  
  def tyyppiVertaus(a: Tuote) = {
    a match {
      case a: Raha => true
      case _ => false
    }
  }
 
  
  def copy(m: Int) = Raha(m)
  
  
  override def toString = {
    määrä + "€"
  }
  
  
}

// Rahan käsittelyn helpottamiseksi.
object Raha {
  
  def apply(määrä: Int) = new Raha(määrä)
  
}


class Vilja(m: Int = 0) extends Tuote(2, m) {
  
  
  def copy(m: Int) = new Vilja(m)
  
  
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







