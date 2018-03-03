package states



abstract class Pelaaja(val tk: Tietokanta) {
  
  // Tällä pelaaja saa vapaudet pelata vuoronsa.
  def vuoro: Vuoro
  
  def voittoIlmoitus(voittaja: Option[Pelaaja], vuoro: Int): Unit
  
  override def toString: String
}


// Tekee jotain älykästä.
class Tekoäly(tk: Tietokanta) extends Pelaaja(tk) {
  def vuoro = {
    myyKaikki()
    ostaPeltoja()
    teeToiminta
    
  }
  override def toString =  "Botti"
  def voittoIlmoitus(voittaja: Option[Pelaaja], vuoro: Int) = {
    Käyttöliittymä.voittoIlmoitus(voittaja, vuoro)
  }
  
  
  private def teeToiminta = {
    val lista = tk.kassa.työLista
    val toimeksianto = (for (i <- 1 to lista.size) yield 1.0).toVector
    new Toiminta(toimeksianto, toimeksianto, tk)
  }
  
  
  private def ostaPeltoja() = {
    val pellot = tk.kartta.laitokset.filter(x => x.työt.
        exists(y => new Viljely(0,0).tyyppiVertaus(y)))
    pellot.foreach(tk.osta(_))
  }
  
  
  private def myyKaikki() = {
    tk.kassa.tuotteet.foreach(x => x.myy(x.määrä, tk.kassa))
  }
  
  
  private def haeRaha = {
    tk.kassa.tuotteet.find(_.tyyppiVertaus(Raha(0)))
  }
  
}


// Testivaiheessa
class Epätekoäly(tk: Tietokanta, nimi: String) extends Pelaaja(tk) {
  
  val vuoronaika = Asetus.asInt("vuoronaika")
  def vuoro: Vuoro = {
    Käyttöliittymä.vuoro(tk, nimi)
    var tulos: Vuoro = new Ohita(tk)
    // Odouspaneeli päivitetään try:n sisällä, jotta pelaaja ei pääsisi
    // interruptaamaan vuoronajan jälkeen.
    try {
      Thread.sleep(vuoronaika)
      tulos = Käyttöliittymä.toimi.getOrElse(tulos)
      Käyttöliittymä.odotusPaneeli()
    } catch {
      case e: InterruptedException => {
        tulos = Käyttöliittymä.toimi.getOrElse(tulos)
        Käyttöliittymä.odotusPaneeli()    
      }
    }
    Käyttöliittymä.päätäVuoro()
    tulos
    

  }
  
  
  override def toString = nimi
  
  // Olennaisempi jos käyttöliittymiä olisi useampia.
  def voittoIlmoitus(voittaja: Option[Pelaaja], vuoro: Int) = {
    Käyttöliittymä.voittoIlmoitus(voittaja, vuoro)
  }

}