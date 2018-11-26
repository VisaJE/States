package states

import java.awt.Image
import java.awt.event._
import java.awt.image.BufferedImage
import java.io.File

import scala.math._
import scala.swing._
import scala.util.Random

import javax.imageio.ImageIO
import javax.swing._


class Kartoittaja(k: Kartta, kokoKerroin: Int = 1) {
  val blokkeja = 10*kokoKerroin
  val miniKoko = 150
  val kartanKoko = 2000
  
  
  private val path = "kartat/ruohoa2.png"
  private val peltop = "kartat/pelto.png"
  private val kaivosp = "kartat/kaivos.png"
  private val tehdasp = "kartat/tehdas.png"
  
  
  
  private val koristeet = Vector(
      "kartat/vaaleepläiskä.png",
      "kartat/puu.png")
      
  private val roskat = Vector(
      "kartat/kivi.png",
      "kartat/lätäkkö.png",
      "kartat/mätäs.png",
      "kartat/puska.png",
      "kartat/kukka.png",
      "kartat/sieni.png",
      "kartat/kanto.png",
      "kartat/tummaruoho.png")
      
  
  def rand = new Random()
  
  
  private def haeKuva(l: Laitos) = {
    val pth = l match {
      case _: Pelto => peltop
      case _: Kaivos => kaivosp
      case _: Tehdas => tehdasp
      case _ => throw AsetusVirhe(message = "Kuva puuttuu laitokselle " +
          l.toString)
    }
    ImageIO.read(new File(pth)).getScaledInstance(kartanKoko/blokkeja, kartanKoko/blokkeja, Image.SCALE_DEFAULT)
  }
  
  
  private def jokuKoriste = {
    val kuva = ImageIO.read(new File(koristeet(rand.nextInt(koristeet.size))))
    kuva.getScaledInstance(kuva.getWidth/kokoKerroin, kuva.getHeight/kokoKerroin, Image.SCALE_DEFAULT)
  }
  // Sijoittaa satunnaiseen kohtaan neliössä.
  private def jokuRoska = {
    val tyhjä = new BufferedImage(kartanKoko/blokkeja, kartanKoko/blokkeja, BufferedImage.TYPE_INT_ARGB)
    var kuva: java.awt.Image = ImageIO.read(new File(roskat(rand.nextInt(roskat.size))))
    if (kokoKerroin != 1) {
    kuva = kuva.getScaledInstance(
            kuva.getWidth(null) /kokoKerroin, kuva.getHeight(null)/kokoKerroin, Image.SCALE_DEFAULT)
    }
    val dx = rand.nextInt(tyhjä.getWidth - kuva.getWidth(null))
    val dy = rand.nextInt(tyhjä.getHeight - kuva.getHeight(null))
    tyhjä.createGraphics().drawImage(kuva, dx, dy, null)
    tyhjä
  }
  
  
  private def koristele() = {
     vapaat.foreach(a => if (rand.nextInt(10) % 5 == 0) a.lisääKoriste(jokuKoriste))
     vapaat.foreach(a => if (rand.nextInt(10) % 2 == 0) a.lisääKoriste(jokuRoska))
  }
  
  // Kartta koostuu neliöistä, joihin voi sijoittua koristeita tai laitoksia.
  
  private val blokit = Array.ofDim[Blokki](blokkeja, blokkeja)
  for (i <- 0 until blokkeja) {
    for (j <- 0 until blokkeja) {
      blokit(j)(i) = new Blokki(j*(kartanKoko/blokkeja), i*(kartanKoko/blokkeja))
    }
  }
  
  
  // Reunat pois käytöstä
  for (i <- 0 until blokit.size) {
    blokit(i)(0).tyhjä = false
    blokit(i)(blokit(0).size-1).tyhjä = false
  }
  blokit(0).foreach(_.tyhjä = false)
  blokit(blokit.size-1).foreach(_.tyhjä = false)
  
  
  
  
  
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
  def createKartta = {
    val tulos = ImageIO.read(new File(path))
    val g2d = tulos.createGraphics()
    koristele()
    blokit.foreach(_.foreach(b => b.ikoni.foreach(i => g2d.drawImage(i, b.xCoord, b.yCoord, null))))
    ImageIO.write(tulos, "PNG", new File("pelikartta.png"))
  }
  
  // private val icon = new ImageIcon(path)
  
  
  
  
   // Laitetaan laitokset blokkeihin
  for (l <- k.laitokset) {
    asetaLaitos(l)
  }
  
  def getKarttaPane = new KarttaPane(this, kartanKoko, miniKoko)
  
  def haeBlokki(x: Int, y: Int): Option[Blokki] = {
    val kerroin = kartanKoko/blokkeja
    val i = x / kerroin
    val j = y / kerroin
    if (j < blokit.size && i < blokit(0).size) {
      Some(blokit(j)(i))
    }
    else None
  }
  
  
  def klikkaa(x: Int, y: Int) = {
    haeBlokki(x, y).foreach(a => Käyttöliittymä.avaaLaitos(a.laitos))
  }
  
}

class KarttaPane(kartoittaja: Kartoittaja, kartanKoko: Int, miniKoko: Int)
  extends JLabel() /*with java.awt.event.MouseListener */{
    
  kartoittaja.createKartta
  setMinimumSize(new Dimension(kartanKoko, kartanKoko))
  setPreferredSize(new Dimension(kartanKoko, kartanKoko))
  setMaximumSize(new Dimension(kartanKoko, kartanKoko))
  setAutoscrolls(true)
  
  val kartta = ImageIO.read(new File("pelikartta.png"))
    
  
  override def paint(g: java.awt.Graphics) = {
    g.drawImage(kartta, 0, 0, null)   }
    
  
  private def miniIcon = ImageIO.read(new File("pelikartta.png"))
    .getScaledInstance(miniKoko, miniKoko, Image.SCALE_DEFAULT)
  
    
  def getMini = new scala.swing.BoxPanel(Orientation.Horizontal) {
    override def paintComponent(g: Graphics2D) {
      g.drawImage(miniIcon, 0, 0, null)
    }
    minimumSize = new Dimension(miniKoko, miniKoko)
    preferredSize = new Dimension(miniKoko, miniKoko)
    maximumSize = new Dimension(miniKoko, miniKoko)
  }
  
  
  val listener = new MouseAdapter() {
    var origin: Point = null
    var coordOrigin: Point = null
    override def mouseClicked(x$1: java.awt.event.MouseEvent): Unit = {
      
    }
    override def mouseDragged(e: java.awt.event.MouseEvent): Unit = {
       if (origin != null) {
         val dx = origin.x - e.getX()
         val dy = origin.y - e.getY()
         Käyttöliittymä.raahaa(dx, dy)
       }
    }  
    override def mousePressed(x$1: java.awt.event.MouseEvent): Unit = {
      origin = x$1.getPoint
      try {
      coordOrigin = x$1.getLocationOnScreen
      }
      catch {
      case e: java.awt.IllegalComponentStateException => coordOrigin = origin
      }
    }
    override def mouseReleased(x$1: java.awt.event.MouseEvent): Unit = {
      try {
        if ( abs(coordOrigin.x - x$1.getXOnScreen) < 10 && abs(coordOrigin.y - x$1.getYOnScreen) < 10) {
          kartoittaja.klikkaa(x$1.getX, x$1.getY)
        }     
      } catch{
        case e : java.awt.IllegalComponentStateException => {}
      }
    }
  }
    

  addMouseListener(listener)
  addMouseMotionListener(listener)
  }


class Blokki(val yCoord: Int,val xCoord: Int) {

  var laitos: Option[Laitos] = None
  
  var ikoni: Option[Image] = None
  
  var tyhjä = true
  
  def lisääLaitos(l: Laitos, i: Image) = {
    laitos = Some(l)
    ikoni = Some(i)
    tyhjä = false
  }
  
  def lisääKoriste(i: Image) = {
    ikoni = Some(i)
    tyhjä = false
  }
  
}
