package states
import scala.collection.mutable.Buffer

object Testailija extends App {
  // Luodaan kassa ja sijoitetaan juttuja.
  val alkuVilja = new Vilja(0)
  val alkuRaha = Raha(100)
  val laitos = new Pelto(Vector(Raha(10)), 10)
  val kaivos = new Kaivos(Vector(Raha(100)), 20)
  val laitos2 = new Pelto(Vector(Raha(50)), 6)
  val kassa = new Kassa(Buffer(alkuRaha), Buffer(laitos))
//  println(kassa + "\n")
//  kassa.lisää(new Vilja(10))
 // kassa.lisää(Raha(10))
 // kassa.kuluta(Vector(Raha(10)))
  //println(kassa + "\n")
 // kassa.laitokset += kaivos
  kassa.laitokset += laitos2
  //println(kassa.kuluta(Vector(Raha(100))))
  //println(kassa)
  
  // Testataan työjärjestelmää
  //val ryhmä = new Työryhmä(1.4, laitos.työt, 3, kassa)
  //println(kassa + " Tyytyväisyys: " + ryhmä.tyytyväisyys)
  //println(kassa.työLista)
  
  // Testataan tietokantaa
  val kartta = new Kartta(Buffer(laitos, kaivos, laitos2))
  val kanta = new Tietokanta(kassa, kartta)
  
  kanta.populaatio = 1
  
  for (i <- 0 to 10) {
    kanta.vuoro(Vector(1.0), Vector(0.8))
    println(kassa)
    println("tyyt " + kanta.tyytyväisyys)
    println("pop " + kanta.populaatio + "\n")
  }
  
  
  
  
  
  
  
  
  
}