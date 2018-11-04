package states

import scala.collection.mutable.Buffer

class Kartta(val laitokset: Buffer[Laitos]) {
  def lisää(laitos: Laitos) = laitokset += laitos
  def lisää(monta: Vector[Laitos]) = laitokset +: monta 
}