package states


// Tuotteen määrä on mutable!
abstract class Tuote(val arvo: Int, private var koko: Int) {

  private def tyypitys[t](r: t) = r
  
  
  def lisää(tuote: Tuote) = {
    if (tyypitys(this) == tyypitys(tuote)) {
    this.koko += tuote.määrä
    true
    }
    else false
  }
  
  
  def riittääkö(tuote: Tuote): Boolean = {
    if (tyypitys(this) != tyypitys(tuote)) throw VääräTuoteVirhe()
    else {
      tuote.määrä >= this.määrä
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

class Raha(määrä: Int = 0) extends Tuote(1, määrä)


class Vilja(määrä: Int = 0) extends Tuote(2, määrä)







