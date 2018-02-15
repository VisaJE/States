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
  
  
  def riittääkö(tuotteet: Vector[Tuote]): Boolean = {
    tuotteet.forall(this.riittääkö(_))
  }
  
  
  private def kuluta(tuote: Tuote): Unit = {
    if (!tuotteet.exists(_.kuluta(tuote))) throw KassaVirhe("Kulutus ristiriita")
  }
  
  
  def kuluta(tuotteet: Vector[Tuote]): Boolean = {
    if (! this.riittääkö(tuotteet)) false
    else {
      tuotteet.foreach(kuluta(_))
      true
    }
  }
}