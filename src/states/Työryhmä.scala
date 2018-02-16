package states

class Työryhmä(ahkeruus: Double, työ: Vector[Työ], koko: Int, kassa: Kassa) {
  
  
  /* Kutsuu lähtökohtaisesti jokaisen työn toimi-metodia. 
   * Jos työryhmän koko ei riitä, listan lopusta töitä jää tekemättä.
   */
  private def toimi(): Int = {
    var toimijat = koko
    var tyytyväisyys = 0
    työ.foreach(
        (x: Työ) => if (x.koko < toimijat && toimijat != 0) {
          tyytyväisyys += x.toimi(ahkeruus, kassa)
          toimijat -= x.koko
        }
        else {
          tyytyväisyys += x.toimi(ahkeruus, kassa, toimijat)
          toimijat = 0
        }
        )
        tyytyväisyys
  }
  
  
  // Työt tehdään samalla kun työryhmä luodaan.
  val tyytyväisyys = toimi()
  
}