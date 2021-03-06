package states


import scala.math._

class Tietokanta(val kassa: Kassa, val kartta: Kartta, var populaatio: Int = 0) {
  
  
  private val tappoIndeksi = Asetus.as("tappoindeksi")
  private val resurssiTyytyväisyysKerroin = Asetus.as("resurssityytyväisyys")
  private val syntyvyysKerroin = Asetus.as("syntyvyysindeksi")
  private val työTyytyväisyysKerroin = Asetus.as("työtyytyväisyyskerroin")
  var tyytyväisyys: Long = 0L
  
  
  def vuoro(työOsuus: Vector[Double], ahkeruus: Vector[Double]) = {
    työllistä(työOsuus, ahkeruus)
    tarveMittaus()
    kuluta()
    syntyvyys()
  }
  
    // Ei tee töitä
  def vuoro() = {
    teeTyöryhmä(populaatio, 1.0, Vector(new Nollatyö(populaatio)))
    tarveMittaus()
    kuluta()
    syntyvyys()
  }
  
  
  def osta(laitos: Laitos): Boolean = {
    if (kassa.riittääkö(laitos.hinta)) {
      if (laitos.osta(this)) {
        kassa.kuluta(laitos.hinta)
        kassa.laitokset += laitos
        true
      }
      else false
    }
    else false
  }
  
  
  
  // Muodostaa työryhmän.
  private def teeTyöryhmä(koko: Int, ahkeruus: Double, työ: Vector[Työ]) = {
    if (populaatio > 0) {
    tyytyväisyys = ((new Työryhmä(ahkeruus, työ, koko, kassa)).tyytyväisyys*työTyytyväisyysKerroin / populaatio + tyytyväisyys).toInt
    }
    
  }
  
  
  // Palauttaa työn suurimman koon.
  private def vapaatPaikat(työ: Vector[Työ]) = työ.foldLeft(0)(_ + _.koko)
  
  
  private def paikkaLista(t: Työ): Vector[Työ] = {
    kassa.työLista.filter(_(0).tyyppiVertaus(t)).flatten
  }
  
  
  private def osuus(a: Double): Int = round(a*populaatio).toInt
  
  // Skaalaa halutut arvot siten, että ne summautuvat ykköseksi.
  private def suhteuta(v: Array[(Boolean, Double)]) = {
    val nyt = v.foldLeft(0.0)( (a: Double, x: (Boolean, Double)) => if (x._1) a + x._2 else a)
    val kerroin = {
      if (nyt > 0) 1.0/nyt
      else 0
    }
    v.map((x: (Boolean, Double)) => if (x._1) (true, kerroin*x._2) else (true, x._2))
  }
 
  
  /* Määrää työt annettujen osuuksien perusteella. Ei jätä ketään työttömäksi, elleivät työpaikat lopu kesken.
   * Mikäli loppuvat, määrää lopun kansan nollatyöhön. Pyrkii noudattamaan annettuja osuuksia muiden alojen
   * kohdalla silloinkin, jos vain osalta aloista loppuu työpaikat. Osuudet skaalataan aluksi ykköseen.
   * Parametrissä on annettava jokainen saatavilla oleva työ nollatyötä lukuunottamatta. Järjestyksen on oltava
   * sama kuin kassan työlistassa.
   */
  def työllistä(v: Vector[Double], ahk: Vector[Double]) = {
    val työt: Vector[Vector[Työ]] = kassa.työLista
    if (työt.size != v.size || v.size != ahk.size) throw PuutteellinenTyölista("Listojen koot eivät täsmää")
    var vapaana  = populaatio
    if (v.size != 0) { 
      var kokoLista = Array.ofDim[Int](v.size)
      // Pari on true, mikäli arvo saa vielä muuttua. 
      // Jos osuudeksi on määrätty 0, työtä ei tehdä missään tilanteessa.
      var osuusLista = v.map(x => if (x == 0) (false, 0.0) else (true, x)).toArray
      while (vapaana > 0 && osuusLista.exists(_._1)) {
        osuusLista = suhteuta(osuusLista)
        kokoLista = osuusLista.map((x: (Boolean, Double)) => osuus(x._2))
        for (i <- 0 until osuusLista.size) {
          val vapaat = vapaatPaikat(työt(i))
          val ehdotus = kokoLista(i)
          if (vapaat <= ehdotus) {
            osuusLista(i) = (false, vapaat.toDouble/populaatio)
            kokoLista(i) = vapaat
          } 
        }
        vapaana = populaatio - kokoLista.reduceLeft(_ + _)
        if (vapaana == 1) {
          val arvo = osuusLista.find(_._1 == true)
          if (arvo != None) {
            kokoLista(osuusLista.indexOf(arvo.get)) += 1
            vapaana = populaatio - kokoLista.reduceLeft(_ + _)
          }
        }
      }
      for (i <- 0 until ahk.size) {
        teeTyöryhmä(kokoLista(i), ahk(i), työt(i))
      }
    }
    if (vapaana > 0) {
      teeTyöryhmä(vapaana, 1.0, Vector(new Nollatyö(vapaana)))
    }
  }
  


  def tarveMittaus() = {
    for (tuote <- kassa.tuotteet) {
      if (populaatio > 0) {
        val arvo = tuote.tarveFunktio(populaatio)
        if (arvo <= 0) {
          populaatio += (arvo*populaatio*tappoIndeksi).toInt
        }
        else {
          tyytyväisyys += (arvo*resurssiTyytyväisyysKerroin).toInt
        }
      }
    }
  }
  
  
  def kuluta() = {
    for (i <- 0 until kassa.tuotteet.size) {
      kassa.tuotteet(i) = kassa.tuotteet(i).käytä(populaatio)
    }
  }
  
  
  def syntyvyys() = {
    populaatio += (syntyvyysKerroin * tyytyväisyys).toInt
    if (populaatio < 0) populaatio = 0
  }
  
}