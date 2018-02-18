package states

import scala.collection.mutable.Buffer
import scala.util.Random
import scala.math._



class Peli(ihmiset: Buffer[String], tekoälyt: Int) {
  private val generaattori = new Random()
  private def satunnainen(max: Int) = generaattori.nextInt(max)
  
  
  val kartta = new Kartta(Buffer())
  
  
  // Määritetään laitokset ja niiden parametrit.
  val peltoteho = 6
  val peltoja = satunnainen(10) + 5
  for (i <- 1 to peltoja) {
    val joku = satunnainen(100)
    kartta.lisää(new Pelto(Vector(Raha(joku+20)), 20 + joku/3, peltoteho))
  }
  
  
  val kaivosteho = 10
  val kaivoksia = satunnainen(8) + 2
  for (i <- 1 to kaivoksia) {
    val joku = satunnainen(200)
    kartta.lisää(new Kaivos(Vector(Raha(joku+50)), 10 + joku, kaivosteho))
  }
  
  
  // Määritetään pelin alkutila
  val alkupopulaatio = 50
  def aloitusesineet: Buffer[Tuote] = Buffer(Raha(150), new Vilja(100))
  
  
 
  // Lisätään aluksi botit
  private var pelaajat: Vector[Pelaaja] = {for ( i <- 1 to tekoälyt) 
    yield new Tekoäly(new Tietokanta(new Kassa(aloitusesineet), kartta, alkupopulaatio))}.toVector
  // Ihmispelaajat. 
  pelaajat = pelaajat ++ {
      for (i <- ihmiset) yield new Epätekoäly(new Tietokanta(
          new Kassa(aloitusesineet), kartta, alkupopulaatio), i)}.toVector
    


  
  // Suorittaa vuorotoiminnan
  var vuoroNumero = 0
  private var voittaja: Option[Pelaaja] = None
  
  
  // TESTI
  pelaajat(0).tk.osta(kartta.laitokset(0))
  pelaajat(1).tk.osta(kartta.laitokset(1))

      
      
  def vuoroilija() = {      
    pelaajat = järjestys(pelaajat)
    if (pelaajat.size == 1) {
      println(pelaajat(0) + " voittaa !!!!")
      voittaja = Some(pelaajat(0))
      }
    if (voittaja == None) {
      vuoroNumero += 1
      val toiminnat: Buffer[Vuoro] = Buffer()
      pelaajat.foreach(toiminnat += _.vuoro)
      toiminnat.foreach(_.suorita())
      loikkaukset()
      
      
    // TESTI 
    pelaajat.foreach((x: Pelaaja) => println(x+ ": " + x.tk.tyytyväisyys + "\n" + x.tk.populaatio + "\n" + x.tk.kassa))     
    }
  }
  
  
  // Järjestää pelaajat tyytyväisyyden mukaan
  private def järjestys(p: Vector[Pelaaja]) = {
    var res = p.sortWith((a: Pelaaja, b: Pelaaja) => 
                if (a.tk.tyytyväisyys == b.tk.tyytyväisyys) {
                  generaattori.nextBoolean()
                }
                else a.tk.tyytyväisyys > b.tk.tyytyväisyys
              )
    res.filter(_.tk.populaatio > 0)
  }
  
  
  // Määrittää loikkaukset tyytyväisyyksien perusteella
  val loikkausIndeksi = 1.0
  val loikkausEksponentti = 2.0
  
  
  private def loikkaukset() = {
    val keskTyyt = pelaajat.foldLeft(0)((a: Int, b: Pelaaja) =>
      a + (b.tk.tyytyväisyys / pelaajat.size))
    pelaajat.foreach((a: Pelaaja) => 
      a.tk.tyytyväisyys += (signum(a.tk.tyytyväisyys - keskTyyt)*
          loikkausIndeksi *
          pow(abs(a.tk.tyytyväisyys - keskTyyt), loikkausEksponentti)).toInt
      )
  }
  
}


// Pelaajien vuoron päätökset tallennetaan suoritettaviksi kerrallaan Command-patternilla. Toistaiseksi vuoron voi käyttää
// vain yhdellä tapaa.
trait Vuoro {
  def suorita()
}


case class Toiminta(työnjako: Vector[Double], ahkeruus: Vector[Double], tk: Tietokanta) extends Vuoro {
  def suorita() = tk.vuoro(työnjako, ahkeruus) 
}
