package states

import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon
import javax.swing.SpinnerNumberModel
import javax.swing.JSpinner
import scala.collection.mutable.Buffer


object Käyttöliittymä extends SimpleSwingApplication {
  
  val peliIkkuna = new MainFrame
  val peliPaneeli = new BoxPanel(Orientation.Vertical)
  peliIkkuna.contents = peliPaneeli
  
  def top = this.peliIkkuna
  
  def vuoro() = println("Nope")
  
  
  val pelaajaNimi  = new TextArea(7, 2) {
      editable = false
      wordWrap = true
      lineWrap = true
      focusable = false
      
    }
  pelaajaNimi.text = "Testi"
  
  
  // Kertoo pelaavan pelaajan nimen.
  val nimiPaneeli = new BoxPanel(Orientation.Horizontal)
  nimiPaneeli.contents += pelaajaNimi
  
  
  
  // Napit pelin pääikkunaan
  val kassaNappi = new Button("KASSA")
  val karttaNappi = new Button("KARTTA")
  val työNappi = new Button("HALLINTA")
  val dataNappi = new Button("HYVINVOINTI")
  val nappiPaneeli = new BoxPanel(Orientation.Horizontal)
  nappiPaneeli.contents += dataNappi
  nappiPaneeli.contents += kassaNappi
  nappiPaneeli.contents += karttaNappi
  nappiPaneeli.contents += työNappi
  nappiPaneeli.contents.foreach(_.focusable = false)
  
  
  
  
  def alustaPääPaneeli() = {
    peliPaneeli.contents.clear()
    peliPaneeli.contents += pelaajaNimi
    peliPaneeli.contents += nappiPaneeli
  }
  peliIkkuna.title = "States"
  
  
  
  
  // Pelin aloitus
  var ihmisiä: Int = 1
  var tekoälyjä: Int = 2 
  
  private def alustaAlkuPaneeli() = {
    peliIkkuna.size = new Dimension(140, 140)
    peliIkkuna.resizable = false
    val pelaajaMalli = new SpinnerNumberModel(2, 2, 10, 1)
    peliPaneeli.contents.clear()
    peliPaneeli.contents += new Label("Epätekoälyjen määrä")
    val spinner1 = new JSpinner(pelaajaMalli)
    peliPaneeli.contents += Component.wrap(spinner1)
    peliPaneeli.focusable = false
    peliPaneeli.contents += new Label("Tekoälyjen määrä")
    val tekoälyMalli = new SpinnerNumberModel(2,2, 10, 1)
    val spinner2 = new JSpinner(tekoälyMalli)
    peliPaneeli.contents += Component.wrap(spinner2)
    peliPaneeli.contents += Button("Seuraava") {
      ihmisiä = spinner1.getValue().##()
      tekoälyjä = spinner2.getValue().##()
      this.nimeäminen()
    }
  }
  
  var nimiLista: Buffer[String] = Buffer()
  private def nimeäminen() = {
    peliIkkuna.size = new Dimension(250, 200)
    peliIkkuna.resizable = false
    peliPaneeli.contents.clear()
    peliPaneeli.contents += new Label("Nimeä pelaajat")

    val nimet = new BoxPanel(Orientation.Vertical)
    val nimiSyöte:Buffer[TextField] = Buffer()
    for (i <- 1 to (ihmisiä)) {
      nimiSyöte += new TextField(i + ". nimi", 20)
    }
    nimiSyöte.foreach(nimet.contents += _)
    nimet.contents += Button("Aloita") {
      nimiLista = nimiSyöte.map(_.text)
      aloita()
    }
    val scrollable = new ScrollPane(nimet)
    peliPaneeli.contents += scrollable
  }
  
  private var peli: Peli = null
  
  private def aloita(): Unit = {
    peli = new Peli(nimiLista, tekoälyjä)
    
  }
  
  
  def voittoIlmoitus(nimi: String) = {
    peliIkkuna.size = new Dimension(40, 80)
    peliIkkuna.resizable = false
    peliPaneeli.contents.clear()
    peliPaneeli.contents += new TextArea(10, 200) {
      editable = false
      focusable = false
      text = nimi + " voittaa!"
    }
    val napit = new BoxPanel(Orientation.Horizontal)
    napit.contents += new Button("Alusta")
    napit.contents += Button("Lopeta") { sys.exit(0) }
    peliPaneeli.contents += napit
    peliPaneeli.focusable = false
  }
  alustaAlkuPaneeli()
  
  
  
  
  
  
  
  
  
  
  
  
}