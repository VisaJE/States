package states


abstract class Tuote(val arvo: Int, val määrä: Int) {

  // Kertoo, kuinka paljon tuotetta tavitaan henkilöä kohden.
  val tarve: Int
      
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
  
  
  def tarveFunktio(populaatio: Int): Double
  
  
  def käytä(populaatio: Int) = {
    var jää = määrä - tarve*populaatio
    if (jää < 0) jää = 0
    this.copy(jää)
  }
  
  
  def myy(myytävä: Int, kassa: Kassa) = {
    if (myytävä <= määrä) {
      kassa.kuluta(Vector(this.copy(myytävä)))
      kassa.lisää(Raha(myytävä*arvo))
    }
  }
}



// Alaluokat

class Raha(m: Int = 0) extends Tuote(1, m) {
  
  val tarve = Asetus.asInt("traha")
  private val tyytyväisyysKerroin = Asetus.as("tkraha")
  
  def tyyppiVertaus(a: Tuote) = {
    a match {
      case a: Raha => true
      case _ => false
    }
  }
 
  
  def tarveFunktio(pop: Int) = {
    määrä*1.0 / pop * tyytyväisyysKerroin
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


class Vilja(m: Int = 0) extends Tuote(Asetus.asInt("avilja"), m) {
  
  val tarve = Asetus.asInt("tvilja")
  private val tyytyväisyysKerroin = Asetus.as("tkvilja")
  
  
  def tarveFunktio(pop: Int) = {
    val arvo = määrä*1.0 / pop - tarve
    if (arvo <= 0) arvo
    else arvo*tyytyväisyysKerroin
  }
  
  
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


class Rauta(m: Int = 0) extends Tuote(Asetus.asInt("arauta"), m) {
 
  val tarve = Asetus.asInt("trauta")
  private val tyytyväisyysKerroin = Asetus.as("tkrauta")
  
  
  def tarveFunktio(pop: Int) = {
    tyytyväisyysKerroin * pop
  }
  
  
  def tyyppiVertaus(a: Tuote) = {
    a match {
      case a: Rauta => true
      case _ => false
    }
  }
 
  
  def copy(m: Int) = new Rauta(m)  
  
  
  override def toString = {
    "Rautaa " + määrä + " leiviskää"
  }  
}


class Työkalut(m: Int = 0) extends Tuote(Asetus.asInt("atyökalut"), m) {
  
  val tarve = Asetus.asInt("ttyökalut")
  private val tyytyväisyysKerroin = Asetus.as("tktyökalut")
  
  
  def tarveFunktio(pop: Int) = {
    tyytyväisyysKerroin * pop
  }
  
  
  def tyyppiVertaus(a: Tuote) = {
    a match {
      case a: Työkalut => true
      case _ => false
    }
  }
  
  def copy(m: Int) = new Työkalut(m)
  
  override def toString = "Työkaluja " + määrä + " kappaletta."
}




