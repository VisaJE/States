package states



abstract class Pelaaja(val tk: Tietokanta) {
  
  // Tällä pelaaja saa vapaudet pelata vuoronsa.
  def vuoro: Toiminta
  
  
  override def toString: String
}


class Tekoäly(tk: Tietokanta) extends Pelaaja(tk) {
  def vuoro = ???
  override def toString =  "Botti"
}


class Epätekoäly(tk: Tietokanta, kl: Käyttöliittymä) extends Pelaaja(tk) {
  def vuoro = ???
  override def toString = ???
}