package states




abstract class Laitos(val hinta: Vector[Tuote], val työt: Vector[Työ]) {
  
  override def toString: String
  
  def hintaString: String = "Vaatii " + {
    if (hinta.size == 1) {
      hinta(0).toString.toLowerCase + "."
    }
    else if (hinta.size == 2) {
      hinta(0).toString.toLowerCase + " ja " +  hinta(1).toString.toLowerCase + "."
    }
    else if (hinta.size > 2) {
      hinta.tail.dropRight(1).foldLeft(hinta(0).toString.toLowerCase)(_  +", "+ _.toString.toLowerCase) +
      " ja " + hinta.last.toString.toLowerCase + "."
    }
  }
  
  private var omistaja: Option[Tietokanta] = None
  
  def omistus = omistaja
  
  def onOmistettu = {
    if (omistaja != None) true
    else false
  }
  
  
  def osta(ostaja: Tietokanta) = if (this.onOmistettu) false
  else {
    omistaja = Some(ostaja)
    true
  }
}


// Alaluokat


// Sekä hinta että tuottavuus määritetään pelikohtaisesti
class Pelto(hinta: Vector[Tuote], koko: Int, teho: Int) extends Laitos(hinta, Vector(new Viljely(koko, teho))) {
  override def toString:String = koko + " asukkaan maatila"
}


class Kaivos(hinta: Vector[Tuote], koko: Int, teho: Int) extends Laitos(hinta, Vector(new Kaivostyö(koko, teho))) {
  override def toString = "Kaivos ja " + koko + " hakkua."
}


class Tehdas(hinta: Vector[Tuote], koko: Int, teho: Int, raudanTarve: Int) extends Laitos(hinta,
    Vector(new Tehtailu(koko, teho, raudanTarve))) {
  override def toString = koko + " työläisen tehdas"
}