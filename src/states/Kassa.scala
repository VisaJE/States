package states
import scala.collection.mutable.Buffer

class Kassa(val tuotteet: Buffer[Tuote], val laitokset: Buffer[Laitos]) {
  
  
  def lisää(tuote: Tuote): Unit = {
     if (!tuotteet.exists(_.lisää(tuote))) {
       tuotteet += tuote
     }
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
    if (!tuotteet.exists((x: Tuote) => 
      try {
      x.kuluta(tuote)
      } catch {
      case e: VääräTuoteVirhe => false
      }
    )) throw KassaVirhe("Kulutus ristiriita")
  }
  
  
  def kuluta(tuotteet: Vector[Tuote]): Boolean = {
    if (! this.riittääkö(tuotteet)) false
    else {
      tuotteet.foreach(kuluta(_))
      true
    }
  }
  
  
  // Lajittelee laitosten työt vektoreihin.
  def työLista: Vector[Vector[Työ]] = {
    val työt = laitokset.map(_.työt).flatten
    var result: Vector[Vector[Työ]] = Vector()
    while (työt.size != 0) {
      val eka = työt.head
      var tämäTyö = Vector(eka)
      työt -= eka
      for (i <- työt) {
        if (i.tyyppiVertaus(eka)) {
          tämäTyö = tämäTyö :+ i
          työt -= i
        }
      }
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