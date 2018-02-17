package states




abstract class Laitos(val hinta: Vector[Tuote], val työt: Vector[Työ]) {
  
  override def toString: String
  
  private var omistaja: Option[Tietokanta] = None
  
  
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