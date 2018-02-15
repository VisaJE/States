package states




class Laitos(val hinta: Vector[Tuote], val työt: Vector[Työ]) {
  
  private var omistaja: Option[String] = None
  
  
  def onOmistettu = {
    if (omistaja != None) true
    else false
  }
  
  
  def osta(ostaja: String) = if (this.onOmistettu) false
  else {
    omistaja = Some(ostaja)
    true
  }
}


// Alaluokat


// Sekä hinta että tuottavuus määritetään pelikohtaisesti
class Pelto(hinta: Vector[Tuote], koko: Int) extends Laitos(hinta, Vector(new Viljely(koko)))