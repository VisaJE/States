  package states

import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import javax.swing.ImageIcon
import javax.swing.SpinnerNumberModel
import javax.swing.JSpinner
import javax.swing.JLabel
import javax.swing.Timer
import javax.swing.JTextField
import javax.swing.JScrollPane
import javax.swing.JLayeredPane
import javax.swing.JPanel
import javax.swing.JViewport
import scala.collection.mutable.Buffer
import scala.concurrent.Future
import javax.swing.SwingUtilities
import java.awt.Graphics2D
import java.io.File
import javax.imageio.ImageIO
import java.util.concurrent._


object Käyttöliittymä extends SimpleSwingApplication {
  
  val peliIkkuna = new MainFrame 
  val peliPaneeli = new BoxPanel(Orientation.Vertical) 
  peliIkkuna.contents = peliPaneeli
  peliIkkuna.title = "States"
  
  def top = this.peliIkkuna
  
  
  val muutIkkunat: Buffer[Frame] = Buffer()
  
  
  private var tk: Tietokanta = null
  var toimi: Option[Vuoro]  = None
  
  
  
  
  def vuoro(tiet: Tietokanta, nimi: String) = {
    tk = tiet
    SwingUtilities.invokeLater(new Runnable() {
      def run() = {    
        pelaajaNimi.text = nimi
        alustaPääPaneeli()
      
      }
    })
  }
  
  

  
  private def määritäToiminta(): Unit = {
    if (ahkeruudet.size != 0) {
      val lista = tk.kassa.työLista
      var ahk = ahkeruudet.map(_.getValue().##()/100.0).toVector
      var osuudet = sliderit.map(_.value/100.0).toVector
      require(ahk.size == osuudet.size)
      val uudet = lista.size - ahk.size
      if (uudet > 0) {
        ahk = ahk ++ Array.ofDim[Double](uudet).toVector
        osuudet = osuudet ++ Array.ofDim[Double](uudet).toVector
      }
      toimi = Some(Toiminta(osuudet, ahk, tk))
    }
  }

  
  
  val pelaajaNimi  = new TextArea() {
      editable = false
      focusable = false
    }
  pelaajaNimi.text = "notanimi"
  
  // Kertoo pelaavan pelaajan nimen.
  val nimiPaneeli = new BoxPanel(Orientation.Vertical) {
    maximumSize = new Dimension(600, 10)
  }
  nimiPaneeli.contents += pelaajaNimi
  
  
  // Ajastin
  val nauhaPath = "art/nauha.png"
  
  
  def ajastin() = {
    val img = ImageIO.read(new File(nauhaPath))
    val leveys = peliPaneeli.size.getWidth
    val kuva = new ImageIcon(img.getScaledInstance((leveys*3).toInt+7,
          30, java.awt.Image.SCALE_SMOOTH)) 
    val paneeli = new BoxPanel(Orientation.Vertical) {
      border = Swing.LineBorder(new Color(50,0,0), 1)
    }
    val pohja = new JScrollPane(new JLabel(kuva),
        javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, 
        javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER
        )
    pohja.setPreferredSize(new Dimension(500, 30))
    pohja.setMinimumSize(new Dimension(500, 30))
    paneeli.contents += Component.wrap(pohja)
    ajastus(pohja)
    paneeli
  }
  
  
  val maksipiste = 460
  val minipiste = 3
  val vuoronaika = Asetus.asInt("vuoronaika")
  val ajastetut: Buffer[ScheduledFuture[_]] = Buffer()
  def ajastus(s: JScrollPane) = {
    val vp = s.getViewport()
    var point = new Point(maksipiste, 0)
    val ex = new ScheduledThreadPoolExecutor(1)
    vp.setViewPosition(point)
 
    val ajastaja = new Runnable {
      def run() = {   
        vp.setViewPosition(point)
        point = new Point((point.getX - 1).toInt, 0)
        s.revalidate()
        s.repaint()
      }
    }
    val väliaika = vuoronaika/(maksipiste-minipiste)
    val toiminto = ex.scheduleAtFixedRate(ajastaja, väliaika, väliaika, TimeUnit.MILLISECONDS)
    ajastetut += toiminto
  }
  
  
  // Taustakuva
  val taustaPath = "art/tausta.png"
  
  val taustaPaneeli = new Component {
    val taustakuva = ImageIO.read(new File(taustaPath)) 
    override def paintComponent(g: Graphics2D) = {
      g.drawImage(taustakuva,0,0, null)
    }
    val width = 600
    val height = 200
    minimumSize = new Dimension(width, height)
    preferredSize = new Dimension(width, height)
    maximumSize = new Dimension(width, height)
  }
  
  
  // Napit pelin pääikkunaan
    val kassaNappi = new Button("KASSA")
    val karttaNappi = new Button("KARTTA")
    val työNappi = new Button("HALLINTA") 
    val päättöNappi = Button("Päätä vuoro") {
        if (sliderit.size == 0) {
          val huom = new Frame() {
            contents = new BoxPanel(Orientation.Vertical) {
            contents += new TextField("Et ole määrittänyt työnjakoa!")
            contents += new BoxPanel(Orientation.Horizontal) {
              contents += Button("Skippaan") {
                peliSäie.interrupt()
                }
              contents += Button("Oho") {
                muutIkkunat.foreach(_.dispose())
                }
              }
            }
            centerOnScreen()
            visible = true
          }
          muutIkkunat += huom
        }
        else {
          määritäToiminta()
          peliSäie.interrupt()
        }
      }
    
    val päättö = new BoxPanel(Orientation.Horizontal) {
        contents += päättöNappi ,
      }
    val superNappiPaneeli = new BoxPanel(Orientation.Vertical)
    val nappiPaneeli = new BoxPanel(Orientation.Horizontal)
    nappiPaneeli.contents += kassaNappi
    nappiPaneeli.contents += karttaNappi
    nappiPaneeli.contents += työNappi
    nappiPaneeli.contents.foreach(_.focusable= false)
    superNappiPaneeli.contents += nappiPaneeli
    superNappiPaneeli.contents += päättö
    superNappiPaneeli.contents.foreach(_.focusable = false)
    superNappiPaneeli.background = new Color(200, 189,  140)
    def napit = {   
    superNappiPaneeli
  }
  
  nappiPaneeli.contents.foreach(listenTo(_))  
  reactions += {
    case ButtonClicked(`kassaNappi`) => alustaKassa()
    case ButtonClicked(`työNappi`) => {
      päättöNappi.background = new Color(100,200,110)
      alustaTyönjako()
    }
    case ButtonClicked(`karttaNappi`) => alustaKartta()
  }
  
  
  // Tähän voisi myös lisätä jonkin kuvan
  private def teeTietoPaneeli = {
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
  
  
  private def alustaPääPaneeli() = {
    peliPaneeli.contents.clear()
    muutIkkunat.foreach(_.dispose())
    peliIkkuna.visible = true
    peliIkkuna.size = new Dimension(460, 460) 
    päättöNappi.background = new Color(200, 100,100)  
    peliPaneeli.contents += taustaPaneeli
      
    peliPaneeli.contents += ajastin
    peliPaneeli.contents += nimiPaneeli
    peliPaneeli.contents += teeTietoPaneeli
    peliPaneeli.contents += napit
    peliIkkuna.centerOnScreen()
    peliIkkuna.visible = true
  }
  
  
  private def alustaKassa(): Unit = {
    muutIkkunat.foreach(_.dispose())
    val kassaIkkuna = new Frame()
    kassaIkkuna.title = "KASSA"
    kassaIkkuna.size = new Dimension (300, 300)
    kassaIkkuna.centerOnScreen()
    val sisältö = new BoxPanel(Orientation.Vertical)
    val scrollable = new ScrollPane(kassaLista)
    sisältö.contents += scrollable
    kassaIkkuna.contents = sisältö
    kassaIkkuna.visible_=(true)
    muutIkkunat += kassaIkkuna
  }
  
  
  // Paneeli, jossa kaikki pelaajan omistamat tuotteet.
  private def kassaLista = {
    val listaPaneeli = new BoxPanel(Orientation.Vertical)
      if (tk.kassa.tuotteet.size > 0) {
         val verrokki = Raha(0)
        for (i <- tk.kassa.tuotteet) {
          val panel = new BoxPanel(Orientation.Horizontal)
          val field = new TextArea(1, 15) {
            text = i.toString
            editable = false
            focusable = false
            border = Swing.EmptyBorder(10, 30, 10, 30)
          }  
          panel.contents += field
          if (!i.tyyppiVertaus(verrokki)) {
            val malli = new SpinnerNumberModel(i.määrä/10, 0, i.määrä, i.määrä/10+1)        
            val spinneri = new JSpinner(malli)
            
            panel.contents += Component.wrap(spinneri)
            panel.contents += Button("Myy") {
              i.myy(spinneri.getValue.##(), tk.kassa)
              alustaKassa()
            }
          }
          panel.border = Swing.LineBorder(new Color(10,10,0), 1)
          listaPaneeli.contents += panel
        }
    }
      else listaPaneeli.contents += new TextField("Ei tunnettuja esineitä.")
    listaPaneeli
  }
  
  
  private def alustaTyönjako() = {
    muutIkkunat.foreach(_.dispose)
    poistaAhkeruudet()
    poistaSliderit()
    val työIkkuna = new Frame()
    muutIkkunat += työIkkuna
    työIkkuna.title = "HALLINTA"
    työIkkuna.size = new Dimension (400, 400)
    työIkkuna.centerOnScreen()
    val sisältö = new BoxPanel(Orientation.Vertical)
    val scrollable = new ScrollPane(työLista)
    sisältö.contents += scrollable
    työIkkuna.contents = sisältö
    työIkkuna.visible_=(true)
    määritäToiminta()
  }
  
  
  // Paneeli, jossa kaikki työt ja niihin liittyvät toiminnot 
  private def työLista:Panel = {
    
    val lista = tk.kassa.työLista
    val työnJako = Array.ofDim[Int](lista.size)
    val paneeli = new BoxPanel(Orientation.Horizontal)
    var indeksi = -1
    var unohdaMuut = false
    if (lista.size > 0) {
      for (i <- lista) {
        indeksi += 1
        val indeksiTässä = indeksi
        val sisäPaneeli = new BoxPanel(Orientation.Vertical)
        sisäPaneeli.border = Swing.LineBorder(new Color(10,10,0), 1)
        
        sisäPaneeli.contents += new TextArea(2,5) {
          text = i(0).toString + "\n" +
          " Koko: " + i.foldLeft(0)(_ + _.koko)
          editable = false
          focusable = false
        }
        
        sisäPaneeli.contents += new Label("Osuus")
        val slideri = lisääSlider()
        sisäPaneeli.contents += slideri
        listenTo(slideri)
        reactions += {
          case ValueChanged(`slideri`) => {
            if (!unohdaMuut) {
              unohdaMuut = true
              asetaSlideri(indeksiTässä, slideri.value)
              unohdaMuut = false
            }
          }
        }
        
        
        val ahkMalli = new SpinnerNumberModel(100, 0, 200, 10)        
        val spinneri = new JSpinner(ahkMalli)
        spinneri.setEditor(new JSpinner.DefaultEditor(spinneri))
        sisäPaneeli.contents += new BoxPanel(Orientation.Vertical) {
          contents += new Label("Panostus")
          contents += Component.wrap(spinneri)
        }
        ahkeruudet += spinneri
        paneeli.contents += sisäPaneeli
      }
      asetaSlideri(0, 1000/sliderit.size)
    }
    else paneeli.contents += new TextField("Töitä ei löydy!", 20)
    paneeli
  }
  
  
  // Työnjaon ahkeruuden hallintaan
  private var ahkeruudet: Buffer[JSpinner] = Buffer()
  
  private def poistaAhkeruudet() = {
    ahkeruudet.clear()
  }
  
  
  // Työnjaon sliderien hallintaan
  private val sliderit: Buffer[Slider] = Buffer()
  private def lisääSlider():Slider = {
    val slider = new Slider {
    min = 1
    max = 1000
    majorTickSpacing = 10
    orientation = Orientation.Vertical
    var väliarvo = 0.0
    }
    sliderit += slider
    slider
  }
  private def poistaSliderit() = sliderit.clear()
  
  
  // Asettaa kaikki sliderit yhden muutoksen mukaan.
  private def asetaSlideri(i: Int, arvo: Int) = {
    val lista = sliderit.map(_.value).toArray.map(x => (false, x))
    lista(i) = (true, arvo)
    val suhteutettu = suhteuta(lista)
    sliderit.zip(suhteutettu).foreach(x => x._1.value = x._2)
  }
  // Skaalaa halutut arvot siten, että ne summautuvat sataseksi. Vain niiden arvoa muutetaan, joiden totuusarvo on false.
  private def suhteuta(v: Array[(Boolean, Int)]) = {
    val loppuSumma = 1000 - v.foldLeft(0)((a,x) => if (x._1) a + x._2 else a)
    val nyt = v.foldLeft(0)( (a, x) => if(!x._1) a + x._2 else a)
    val kerroin = {
      if (nyt > 0) loppuSumma*1.0/nyt
      else 0.0
    }
    v.map(x => if (!x._1) scala.math.round((kerroin*x._2)).toInt else x._2)
  }
  
  // Laitosten tiedot tulevat tähän.
  val yliLaitosPaneeli = new BoxPanel(Orientation.Vertical){
      maximumSize = new Dimension(160, 1000)
    }
  
    // Kartta
  private def alustaKartta() = {
    muutIkkunat.foreach(_.dispose())
    val karttaIkkuna = new Frame()
    muutIkkunat += karttaIkkuna
    karttaIkkuna.visible = true
    karttaIkkuna.minimumSize = new Dimension(800, 500)
    karttaIkkuna.resizable = true
    karttaIkkuna.title = "KARTTA"
    
    val karttaPaneeli = new BoxPanel(Orientation.Horizontal)
    karttaIkkuna.contents = karttaPaneeli
    val tietoKohta = new BoxPanel(Orientation.Vertical)
    tietoKohta.preferredSize = new Dimension(160, 400)
    tietoKohta.contents += miniKartta
    
    
    yliLaitosPaneeli.contents.clear()
    yliLaitosPaneeli.contents += nullLaitosPaneeli
    tietoKohta.contents += yliLaitosPaneeli
    tietoKohta.border_=(Swing.LineBorder(new Color(100, 100, 100), 2))
    
    
    // Kuvallinen kartta
    
    val scrollable = new JScrollPane(kkartta) {
      setMinimumSize(new Dimension(550, 500))
    }
    karttaPaneeli.contents += Component.wrap(scrollable)
    karttaPaneeli.contents += tietoKohta
    karttaPaneeli.revalidate
    karttaPaneeli.repaint
    karttaIkkuna.centerOnScreen()
  }
  
  
  def raahaa(dx: Int, dy: Int) = {
    val portti: JViewport =  SwingUtilities
    .getAncestorOfClass(new (JViewport).getClass, kkartta).asInstanceOf[JViewport]
    if (portti != null) {
      val kuva = portti.getViewRect()
      kuva.x += dx
      kuva.y += dy
      kkartta.scrollRectToVisible(kuva)
    }
  }
  
  
  def avaaLaitos(l: Option[Laitos]) = {
    yliLaitosPaneeli.contents.clear()
    yliLaitosPaneeli.contents += {
      if (l != None) laitosPaneeli(l.get) else nullLaitosPaneeli
    }
    yliLaitosPaneeli.revalidate
    yliLaitosPaneeli.repaint
  }
  
  
  // Laitoksesta saatava info ja ostomahdollisuus paneelissa.
  private def laitosPaneeli(laitos: Laitos): Panel = {
    val paneeli = new BoxPanel(Orientation.Vertical)
    val omistaja = laitos.omistus
    paneeli.contents += new TextArea(2, 3) {
      text = laitos.toString
      editable = false
      focusable = false
      border = Swing.LineBorder(new Color(0,0,0),1)
      lineWrap = true
      wordWrap = true
    }
    paneeli.contents += new TextArea(1,3) {
      editable = false
      focusable = false
      lineWrap = true
      wordWrap = true
      border = Swing.LineBorder(new Color(0,0,0), 1)
      text = {
        if (omistaja != None) {
          "Ei myynnissä."
        }
        else "Vapailla markkinoilla."
      }
    }
    if (omistaja == None)  {
      paneeli.contents += new TextArea(laitos.hintaString) {
        lineWrap = true
        wordWrap = true
        editable = false
        focusable = false
        border = Swing.LineBorder(new Color(0,0,0), 1)
      }
      paneeli.contents += new BorderPanel {
        layout(Button("Osta") {
          if (!tk.osta(laitos)) {
            paneeli.contents.clear()
            paneeli.contents += new TextField("Resurssit eivät riitä." , 20)
            paneeli.revalidate
            paneeli.repaint
          } 
          else {
            paneeli.contents.clear()
            paneeli.contents += new TextField("Ostettu!", 20)
            paneeli.revalidate
            paneeli.repaint
          }
        }) = Center
        maximumSize = new Dimension(150, 50)
      }
    }
    paneeli.maximumSize = new Dimension(150, 1000)
    paneeli
  }
  
  private def nullLaitosPaneeli: Panel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += new TextArea(2, 2) {
        text = tk.kassa.toString
        editable = false
        focusable = false
        border = Swing.LineBorder(new Color(0,0,0), 1)
      }
    }
  }
  
  
  
  def päätäVuoro() = {
    ajastetut.foreach(_.cancel(false))
    muutIkkunat.foreach(frame => frame.dispose())
    tk = null
    pelaajaNimi.text = "notaname"
    toimi = None
    poistaAhkeruudet()
    poistaSliderit()
    
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
    peliIkkuna.centerOnScreen()
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
      odotusPaneeli()
      aloita()
    }
    val scrollable = new ScrollPane(nimet)
    peliPaneeli.contents += scrollable
    peliIkkuna.centerOnScreen()
  }
  
  
  def odotusPaneeli() = {
    SwingUtilities.invokeLater(new Runnable() {
      def run() = {    
        peliIkkuna.visible = false
        peliIkkuna.size = new Dimension(300, 300)
        peliPaneeli.contents.clear()
        val odotus = new Frame() {
          size = new Dimension(300, 300)
        }
        odotus.contents = new TextField("Odotetaan muita pelaajia. . .", 20) {
        editable = false
        }
        odotus.visible = true
        odotus.resizable = false
        odotus.centerOnScreen()
        muutIkkunat += odotus
      }
    })
  }
  
  
  
  private var peliSäie: Thread = null
  
  
  private def aloita(): Unit = {
    peliSäie = new Thread(new PeliSäie)
    peliSäie.start()
    
  }
  
  
  private var peli: Peli = null
  private var kkartta: KarttaPane = null
  private var miniKartta: swing.Component = null
  
  class PeliSäie extends Runnable {
    def run = {
      peli = new Peli(nimiLista, tekoälyjä)
      val kartoittaja = (new Kartoittaja(peli.kartta, peli.laitoksia/60+1))
      kkartta = kartoittaja.getKarttaPane
      miniKartta = kkartta.getMini 
      peli.aloita()
    }
  }
  
  
  private var ilmoitettu = false
  
  // Erillinen metodi pääsäikeen valintaan.
  def voittoIlmoitus(v: Option[Pelaaja], vuoro: Int) = {
    SwingUtilities.invokeLater(new Runnable() {
      def run() = {    
        säikeelläIlmoitus(v, vuoro)
      }
    })
  }
  
  
  private def säikeelläIlmoitus(voittaja: Option[Pelaaja], vuoro: Int) = {
    muutIkkunat.foreach(_.dispose())
    peliIkkuna.visible = true
    if (!ilmoitettu) {
      peliPaneeli.contents.clear()
      peliIkkuna.size = new Dimension(200, 150)
      val teksti = {
        if (voittaja == None) "Tasapeli. (Kaikki oli huonoja.)\nVuoroja pelattu " +
        vuoro +"."
        else voittaja.get.toString + " voittaa!\nVuoroja pelattu " + vuoro + "."
      }
      peliPaneeli.contents += new TextArea(teksti) {
        editable = false
        focusable = false
        wordWrap = true
        lineWrap = true
        text = teksti
      }
      val napit = new BoxPanel(Orientation.Horizontal)
      napit.contents += Button("Alusta") {
        muutIkkunat.foreach(_.dispose())
        tk = null
        toimi = None
        ilmoitettu = false
        alustaAlkuPaneeli()
      }
      napit.contents += Button("Lopeta") { sys.exit(0) }
      peliPaneeli.contents += napit
      peliPaneeli.focusable = false
    }
    ilmoitettu = true
  }
  
  
  // Tästä lähtee
  alustaAlkuPaneeli()
  
  
}


