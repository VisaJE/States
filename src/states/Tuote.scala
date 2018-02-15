package states

abstract class Tuote(val arvo: Int, var määrä: Int) {

  private def tyypitys[t](r: t) = r
  
  
  def lisää(tuote: Tuote) = {
    if (tyypitys(this) == tyypitys(tuote)) {
    this.määrä += tuote.määrä
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
        this.määrä -= tuote.määrä
        true
      }
      else false
  }
  
  
}

class Raha(määrä: Int = 0) extends Tuote(1, määrä)







