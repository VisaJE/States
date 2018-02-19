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
    new Toiminta(Vector(), Vector(), tk)
  }
  override def toString =  "Botti"
  def voittoIlmoitus(voittaja: Option[Pelaaja]) = {}
  
}


// Testivaiheessa
class Epätekoäly(tk: Tietokanta, nimi: String) extends Pelaaja(tk) {
  
  def vuoro: Vuoro = {
    Käyttöliittymä.vuoro(tk, nimi)
    Thread.sleep(20000)
    Käyttöliittymä.toimi.getOrElse(new Ohita(tk))
  }
  
  
  override def toString = nimi
  
  // Koska käyttöliittymiä on vain yksi
  private var ilmoitettu = false
  def voittoIlmoitus(voittaja: Option[Pelaaja]) = {
    
    if (!ilmoitettu) {
    Käyttöliittymä.voittoIlmoitus(voittaja)
    ilmoitettu = true
    }
  }
}