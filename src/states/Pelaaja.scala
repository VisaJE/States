package states



abstract class Pelaaja(val tk: Tietokanta) {
  
  // Tällä pelaaja saa vapaudet pelata vuoronsa.
  def vuoro: Toiminta
  
  def voittoIlmoitus(nimi: String): Unit
  
  override def toString: String
}


// Testivaiheessa
class Tekoäly(tk: Tietokanta) extends Pelaaja(tk) {
  def vuoro = {
    Käyttöliittymä.alustaPääPaneeli()
    new Toiminta(Vector(1.0), Vector(1.5), tk)
  }
  override def toString =  "Botti"
  def voittoIlmoitus(nimi: String) = Käyttöliittymä.voittoIlmoitus(nimi)
  
}


// Testivaiheessa
class Epätekoäly(tk: Tietokanta, nimi: String) extends Pelaaja(tk) {
  def vuoro: Toiminta = new Toiminta(Vector(1.0), Vector(1.7), tk)
  override def toString = "nimi"
  def voittoIlmoitus(nimi: String) = {}
}