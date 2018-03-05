package states

import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon
import javax.imageio.ImageIO
import java.io.File
import javax.swing._
import java.awt.Image
import java.awt.image.BufferedImage
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
      "kartat/puu.png")
  
      
  private def haeKuva(l: Laitos) = {
    val pth = l match {
      case _: Pelto => peltop
      case _: Kaivos => kaivosp
      case _: Tehdas => tehdasp
      case _ => throw AsetusVirhe(message = "Kuva puuttuu laitokselle " +
          l.toString)
    }
    ImageIO.read(new File(pth))
  }
  
  
      
  
  // Kartta koostuu neliöistä, joihin voi sijoittua koristeita tai laitoksia.
  private val blokit = Array.ofDim[Blokki](20, 20)
  for (i <- 0 to 19) {
    for (j <- 0 to 19) {
      blokit(j)(i) = new Blokki(j*100, i*100)
    }
  }
  
  
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
    jokuBlokki.lisääLaitos(l, haeKuva(l))
  }
  
  
  // Lataillaan kartta ja lisätään ikonit
  private def kartta = {
    val tulos = ImageIO.read(new File(path))
    val g2d = tulos.createGraphics()
    blokit.foreach(_.foreach(b => b.ikoni.foreach(i => g2d.drawImage(i, b.xCoord, b.yCoord, null))))
    ImageIO.write(tulos, "PNG", new File("pelikartta.png"))
  }
  
  // private val icon = new ImageIcon(path)
  private val miniIcon = ???//kartta.getScaledInstance(100, 100, Image.SCALE_DEFAULT)
  
  
   // Laitetaan laitokset blokkeihin
  for (l <- k.laitokset) {
    asetaLaitos(l)
  }
  
  
  def getKartta = new Component {
    
    minimumSize = new Dimension(2000, 2000)
    preferredSize = new Dimension(2000, 2000)
    maximumSize = new Dimension(2000, 2000)
    
    override def paintComponent(g: Graphics2D) = {
    }
  }
  
  
  def getMini = new scala.swing.BoxPanel(Orientation.Horizontal) {
    override def paintComponent(g: Graphics2D) {
      g.drawImage(miniIcon, 0, 0, null)
    }
    minimumSize = new Dimension(100, 100)
    preferredSize = new Dimension(100, 100)
    maximumSize = new Dimension(100, 100)
  }
  
}


class Blokki(val yCoord: Int,val xCoord: Int) {

  var laitos: Option[Laitos] = None
  
  var ikoni: Option[BufferedImage] = None
  
  var tyhjä = true
  
  def lisääLaitos(l: Laitos, i: BufferedImage) = {
    laitos = Some(l)
    ikoni = Some(i)
  }
  
}