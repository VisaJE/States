package states

import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon
import javax.swing.SpinnerNumberModel
import javax.swing.JSpinner
import javax.swing.Timer
import scala.collection.mutable.Buffer
import scala.swing.event.ButtonClicked
import scala.concurrent.Future



object Käyttöliittymä extends SimpleSwingApplication {
  
  val peliIkkuna = new MainFrame {
    peer.setLocationRelativeTo(null)
  }
  val peliPaneeli = new BoxPanel(Orientation.Vertical) 
  peliIkkuna.contents = peliPaneeli
  peliIkkuna.title = "States"
  
  def top = this.peliIkkuna
  
  
  
  private var tk: Tietokanta = null
  private var toimi: Vuoro  = null
  

  
  
  def vuoro(tiet: Tietokanta, nimi: String): Vuoro = {
    this.tk = tiet
    pelaajaNimi.text = nimi
    val nappi = Käyttöliittymä.alustaPääPaneeli()
    new Thread(new Kuuntelu(nappi)).start()
    lueToimi
  }
  
  // Aikarajan toimintaan.
  class Kuuntelu(nappi: Button) extends Runnable {
    def run() =  synchronized {
      listenTo(nappi)
      reactions += {
      case ButtonClicked(nappi) => {
        toimi =  new Ohita(tk)
        notifyAll()
      }
    }
    }
  }
  
    def lueToimi = synchronized {
    while (toimi == null) {
      this.wait()
    }
    toimi
  }
  
  
  val pelaajaNimi  = new TextArea(7, 2) {
      editable = false
      wordWrap = true
      lineWrap = true
      focusable = false
      
    }
  pelaajaNimi.text = "notanimi"
  
  
  // Kertoo pelaavan pelaajan nimen.
  val nimiPaneeli = new BoxPanel(Orientation.Horizontal)
  nimiPaneeli.contents += pelaajaNimi
  
  
  
  // Napit pelin pääikkunaan
    def teeNapit = {
    val kassaNappi = new Button("KASSA")
    val karttaNappi = new Button("KARTTA")
    val työNappi = new Button("HALLINTA")
    val nappiPaneeli = new BoxPanel(Orientation.Horizontal)
    nappiPaneeli.contents += kassaNappi
    nappiPaneeli.contents += karttaNappi
    nappiPaneeli.contents += työNappi
    nappiPaneeli.contents.foreach(_.focusable = false)
    nappiPaneeli
  }
  
  
  def teeTietoPaneeli = {
    val paneeli = new BoxPanel(Orientation.Vertical)
    
    val pop = "Kansan populaatio: " + tk.populaatio +"."
    paneeli.contents += new TextField(pop, 20) {
      focusable = false
      editable = false
    }
    
    val tyyt = "Kansan keskimääräinen tyytyväisyys: " + tk.tyytyväisyys+"."
    paneeli.contents += new TextField(tyyt, 20) {
      focusable = false
      editable = false
    }
    paneeli
  }
  
  
  def alustaPääPaneeli() = {
    peliPaneeli.contents.clear()
    peliIkkuna.size = new Dimension(460, 460)  
    peliPaneeli.contents += pelaajaNimi
    peliPaneeli.contents += teeTietoPaneeli
    peliPaneeli.contents += teeNapit
    val päättöNappi = Button("Päätä vuoro") {
      odotusPaneeli()
      päätäVuoro()
    }
    peliPaneeli.contents += päättöNappi
    päättöNappi
  }
  
  
  
  def päätäVuoro() = {
    tk = null
    pelaajaNimi.text = "notaname"
  }
  
  
  // Pelin aloitus
  var ihmisiä: Int = 1
  var tekoälyjä: Int = 2 
  
  private def alustaAlkuPaneeli() = {
    peliIkkuna.size = new Dimension(250, 164)
    peliIkkuna.resizable = false
    peliPaneeli.contents.clear()
    peliPaneeli.focusable = false
    

    val otsikko = new TextField("Pelaajien määrä", 24) {
      editable = false
      focusable = false
    }
    peliPaneeli.contents += otsikko
    
    val pelaajaMalli = new SpinnerNumberModel(1, 0, 10, 1)
    peliPaneeli.contents += new Label("Epätekoälyt")
    val spinner1 = new JSpinner(pelaajaMalli)
    spinner1.setEditor(new JSpinner.DefaultEditor(spinner1))
    peliPaneeli.contents += Component.wrap(spinner1)
    
    peliPaneeli.contents += new Label("Tekoälyt")
    val tekoälyMalli = new SpinnerNumberModel(1,0, 10, 1)
    val spinner2 = new JSpinner(tekoälyMalli)
    spinner2.setEditor(new JSpinner.DefaultEditor(spinner2))
    peliPaneeli.contents += Component.wrap(spinner2)
    
    peliPaneeli.contents += Button("Seuraava") {
      ihmisiä = spinner1.getValue().##()
      tekoälyjä = spinner2.getValue().##()
      if (ihmisiä + tekoälyjä >= 2) {
      this.nimeäminen()
      } else otsikko.text = "Pelaajia oltava vähintään kaksi."
    }
    // Koristelua
    peliPaneeli.border = Swing.LineBorder(new Color(10,10,0), 2)
    peliPaneeli.background = new Color(200, 189,  140)
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
      peliPaneeli.contents.clear()
      aloita()
    }
    val scrollable = new ScrollPane(nimet)
    peliPaneeli.contents += scrollable
  }
  
  
  def odotusPaneeli() = {
    peliPaneeli.contents.clear()
    peliIkkuna.size = new Dimension(300, 100)
    peliPaneeli.contents += new TextField("Odotetaan muita pelaajia. . .", 20) {
      editable = false
    }
  }
  
  
  private var peli: Peli = null
  
  private def aloita(): Unit = {
    peli = new Peli(nimiLista, tekoälyjä)
  }
  
  
  def voittoIlmoitus(voittaja: Option[Pelaaja]) = {
    peliIkkuna.size = new Dimension(40, 80)
    peliIkkuna.resizable = false
    peliPaneeli.contents.clear()
    val teksti = {
      if (voittaja == None) "Tasapeli." 
      else voittaja.get.toString + " voittaa!"
    }
    peliPaneeli.contents += new TextArea(10, 200) {
      editable = false
      focusable = false
      text = teksti
    }
    val napit = new BoxPanel(Orientation.Horizontal)
    napit.contents += new Button("Alusta")
    napit.contents += Button("Lopeta") { sys.exit(0) }
    peliPaneeli.contents += napit
    peliPaneeli.focusable = false
  }
  
  
  // Tästä lähtee
  alustaAlkuPaneeli()
  
  
  
}


