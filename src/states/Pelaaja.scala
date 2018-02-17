package states



abstract class Pelaaja(val tk: Tietokanta) {
  
  // Tällä pelaaja saa vapaudet pelata vuoronsa.
  def vuoro: Toiminta
  
  
  override def toString: String
}


// Testivaiheessa
class Tekoäly(tk: Tietokanta) extends Pelaaja(tk) {
  def vuoro = new Toiminta(Vector(1.0), Vector(1.5), tk)
  override def toString =  "Botti"
}


// Testivaiheessa
class Epätekoäly(tk: Tietokanta, kl: Option[Käyttöliittymä]) extends Pelaaja(tk) {
  def vuoro = new Toiminta(Vector(1.0), Vector(1.7), tk)
  override def toString = "TestiJorma"
}