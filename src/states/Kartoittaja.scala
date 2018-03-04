package states

import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon
import javax.imageio.ImageIO
import java.io.File
import javax.swing._
import java.awt.Image
import scala.util.Random


class Kartoittaja(k: Kartta) {
  private val path = "kartat/ruohoa.png"
  private val peltop = "kartat/pelto.png"
  private val kaivosp = "kartat/kaivos.png"
  private val tehdasp = "kartat/tehdas.png"
  
  
  
  private val koristeet = Vector(
      "kartat/vaaleepläiskä.png",
      "kartat/laavaa.png",
      "kartat/kivi.png",
      "kartat/lätäkkö.png",
      "kartat/puu.png"
      )
  
  
  // Kartta koostuu neliöistä, joihin voi sijoittua koristeita tai laitoksia.
  private val blokit = Array.ofDim[Blokki](20, 20).map(_.map(_ =>new Blokki))
  
  
  // Reunat pois käytöstä
  for (i <- 0 until blokit.size) {
    blokit(i)(0).tyhjä = false
    blokit(i)(blokit(0).size-1).tyhjä = false
  }
  blokit(0).foreach(_.tyhjä = false)
  blokit(blokit.size-1).foreach(_.tyhjä = false)
  
  
  def rand = new Random()
  
  
  def vapaat = blokit.flatten.filter(_.tyhjä == true)
  
  
  def jokuBlokki = {
    val v = vapaat
    if (v.size > 0) {
      v(rand.nextInt(v.size))
     } else throw AsetusVirhe(message = "Kartalle ei mahdu kaikkea")
  }
  
  
  def asetaLaitos(l: Laitos) = {
    jokuBlokki.lisääLaitos(l)
  }
  
  
  
  private val kartta = ImageIO.read(new File(path))
  private val icon = new ImageIcon(path)
  private val miniIcon = kartta.getScaledInstance(100, 100, Image.SCALE_DEFAULT)
  
  
  def getKartta = new JLabel(icon)
  
  
  def getMini = new scala.swing.BoxPanel(Orientation.Horizontal) {
    override def paintComponent(g: Graphics2D) {
      g.drawImage(miniIcon, 0, 0, null)
    }
    minimumSize = new Dimension(100, 100)
    preferredSize = new Dimension(100, 100)
    maximumSize = new Dimension(100, 100)
  }
  
}

class Blokki {
  var laitos: Option[Laitos] = None
  
  var ikoni: Option[Graphics2D] = ???
  
  var tyhjä = true
  
  // Hakee myös ikonin
  def lisääLaitos(l: Laitos) = {
    laitos = Some(l)
  }
  
}