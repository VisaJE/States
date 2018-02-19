package states



abstract class Pelaaja(val tk: Tietokanta) {
  
  // Tällä pelaaja saa vapaudet pelata vuoronsa.
  def vuoro: Vuoro
  
  def voittoIlmoitus(voittaja: Option[Pelaaja]): Unit
  
  override def toString: String
}


// Testivaiheessa
class Tekoäly(tk: Tietokanta) extends Pelaaja(tk) {
  def vuoro = {
    // Testailijalle
    //new Toiminta(Vector(1.0), Vector(1.5), tk)
    // Peli-luokalle
    Thread.sleep(2000)
    new Ohita(tk)
  }
  override def toString =  "Botti"
  def voittoIlmoitus(voittaja: Option[Pelaaja]) = {}
  
}


// Testivaiheessa
class Epätekoäly(tk: Tietokanta, nimi: String) extends Pelaaja(tk) {
  
  def vuoro: Vuoro = {
    Käyttöliittymä.vuoro(tk, nimi)
    try {
      Thread.sleep(20000)
    } catch {
      case e: InterruptedException => {}
    }
    val tulos = Käyttöliittymä.toimi.getOrElse(new Ohita(tk))
    Käyttöliittymä.odotusPaneeli()
    Käyttöliittymä.päätäVuoro()
    tulos
  }
  
  
  override def toString = nimi
  
  // Olennaisempi jos käyttöliittymiä olisi useampia.
  def voittoIlmoitus(voittaja: Option[Pelaaja]) = {
    Käyttöliittymä.voittoIlmoitus(voittaja)
  }
  
}