package states

import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon
import javax.imageio.ImageIO
import java.io.File
import javax.swing._
import java.awt.Image


class Kartoittaja(k: Kartta) {
  private val path = "kartat/testikartta.png"
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