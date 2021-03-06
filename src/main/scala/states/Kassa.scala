package states
import scala.collection.mutable.Buffer

class Kassa(val tuotteet: Buffer[Tuote] = Buffer(), val laitokset: Buffer[Laitos] = Buffer()) {
  
  
  def lisää(tuote: Tuote): Unit = {
     val indeksi = löytyykö(tuote)
     if (indeksi > -1) tuotteet(indeksi) = tuotteet(indeksi).lisää(tuote)
     else tuotteet +=  tuote
  }
  
  
  def löytyykö(t: Tuote) = {
    tuotteet.map(_.tyyppiVertaus(t)).indexOf(true)
  }
  
  
  def lisää(tuotteet: Vector[Tuote]): Unit = {
    tuotteet.foreach((x: Tuote) => this.lisää(x) )
  }
  
  
  def riittääkö(tuote: Tuote): Boolean = {
    
    tuotteet.exists((x: Tuote) => try {
      x.riittääkö(tuote)
    } catch {
      case e: VääräTuoteVirhe => false
    }
    )
    
  }
  
  
  // Tuotteet saavat esiintyä vain kerran listassa.
  def riittääkö(tuotteet: Vector[Tuote]): Boolean = {
    tuotteet.forall(this.riittääkö(_))
  }
  
  
  private def kuluta(tuote: Tuote): Unit = {
      val indeksi = löytyykö(tuote)
      tuotteet(indeksi) = tuotteet(indeksi).kuluta(tuote) 
  }
  
  
  def kuluta(tuotteet: Vector[Tuote]): Boolean = {
    if (! this.riittääkö(tuotteet)) false
    else {
      tuotteet.foreach(kuluta(_))
      true
    }
  }
  
  
  // Lajittelee laitosten työt vektoreihin. For-loopin bugin takia samat työt
  // etsitään aluksi tämäTyö listaan ja poistetaan sitten erikseen töistä.
  def työLista: Vector[Vector[Työ]] = {
    var työt = laitokset.map(_.työt).flatten
    var result: Vector[Vector[Työ]] = Vector()
    while (työt.size != 0) {
      val eka = työt.head
      var tämäTyö = Vector(eka)
      työt -= eka
      for (i <- työt
          if (eka.tyyppiVertaus(i))) {       
          tämäTyö = tämäTyö :+ i
          }
      työt = työt.filterNot(_.tyyppiVertaus(eka))
      result = result :+ tämäTyö
    }
    result
  }
  
  
  override def toString = {
    var text = "Tuotteet:\n"
    for (i <- tuotteet) {
      text += i.toString + "\n"
    }
    text += "Laitokset:\n"
    for (i <- laitokset) {
      text += i.toString + "\n"
    }
    text
  }
  
  
}