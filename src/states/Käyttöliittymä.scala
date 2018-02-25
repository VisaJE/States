  package states

import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon
import javax.swing.SpinnerNumberModel
import javax.swing.JSpinner
import javax.swing.Timer
import javax.swing.JTextField
import scala.collection.mutable.Buffer
import scala.swing.event.ButtonClicked
import scala.concurrent.Future
import javax.swing.SwingUtilities



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
    SwingUtilities.invokeAndWait(new Runnable() {
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

  
  
  val pelaajaNimi  = new TextArea(1, 5) {
      editable = false
      focusable = false
    }
  pelaajaNimi.text = "notanimi"
  
  // Kertoo pelaavan pelaajan nimen.
  val nimiPaneeli = new BoxPanel(Orientation.Vertical)
  nimiPaneeli.contents += pelaajaNimi
  
  
  // Napit pelin pääikkunaan
    val kassaNappi = new Button("KASSA")
    val karttaNappi = new Button("KARTTA")
    val työNappi = new Button("HALLINTA")
    val päättöNappi = Button("Päätä vuoro") {
        määritäToiminta()
        peliSäie.interrupt()
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
    case ButtonClicked(`työNappi`) => alustaTyönjako()
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
      peliIkkuna.size = new Dimension(460, 460)  
      peliPaneeli.contents += nimiPaneeli
      peliPaneeli.contents += teeTietoPaneeli
      peliPaneeli.contents += napit
      peliIkkuna.centerOnScreen()
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
            val malli = new SpinnerNumberModel(i.määrä/10, 0, i.määrä, 10)        
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
            asetaSlideri(indeksiTässä, slideri.value)
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
    max = 100
    majorTickSpacing = 1
    orientation = Orientation.Vertical
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
    val loppuSumma = 100 - v.foldLeft(0)((a,x) => if (x._1) a + x._2 else a)
    val nyt = v.foldLeft(0)( (a, x) => if(!x._1) a + x._2 else a)
    val kerroin = {
      if (nyt > 0) loppuSumma*1.0/nyt
      else 0
    }
    v.map(x => if (!x._1) (kerroin*x._2).toInt else x._2)
  }
  
  
  
    // Kartta
  private def alustaKartta() = {
    muutIkkunat.foreach(_.dispose())
    val karttaIkkuna = new Frame()
    muutIkkunat += karttaIkkuna
    karttaIkkuna.visible = true
// EI toimi toivotusti    karttaIkkuna.centerOnScreen()
    karttaIkkuna.minimumSize = new Dimension(500, 500)
    karttaIkkuna.resizable = false
    karttaIkkuna.title = "KARTTA"
    
    val karttaPaneeli = new BoxPanel(Orientation.Horizontal)
    karttaIkkuna.contents = karttaPaneeli
    val tietoKohta = new BoxPanel(Orientation.Vertical)
    tietoKohta.contents += nullLaitosPaneeli
    tietoKohta.border_=(Swing.LineBorder(new Color(100, 100, 100), 2))
    
    
    // Väliaikainen kartta
    val kartta = new BoxPanel(Orientation.Vertical)
    karttaPaneeli.contents += kartta
    for (i <- tk.kartta.laitokset) {
      kartta.contents += Button("Laitos") {
        tietoKohta.contents.clear()
        tietoKohta.contents += laitosPaneeli(i)
        tietoKohta.revalidate
        tietoKohta.repaint
      }
    }
    karttaPaneeli.contents += tietoKohta
  }
  
  
  // Laitoksesta saatava info ja ostomahdollisuus paneelissa.
  private def laitosPaneeli(laitos: Laitos): Panel = {
    val paneeli = new BoxPanel(Orientation.Vertical)
    val omistaja = laitos.omistus
    paneeli.contents += new TextArea(2, 5) {
      text = laitos.toString
      editable = false
      focusable = false
      border = Swing.LineBorder(new Color(10,10,0),1)
    }
    paneeli.contents += new TextArea(1,1) {
      editable = false
      focusable = false
      text = {
        if (omistaja != None) {
          "Omistaja: " + omistaja.get
        }
        else "Vapailla markkinoilla."
      }
    }
    if (omistaja == None)  {
      paneeli.contents += new TextField(laitos.hintaString, 20)
      paneeli.contents += Button("Osta") {
        if (!tk.osta(laitos)) {
          paneeli.contents += new TextField("Osto epäonnistui." , 20)
          paneeli.revalidate
          paneeli.repaint
        }
        else alustaKartta()
      }
    }
    paneeli
  }
  
  private def nullLaitosPaneeli: Panel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += new TextArea(5, 5) {
        text = ""
        editable = false
        focusable = false
      }
    }
  }
  
  
  def päätäVuoro() = {
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
        peliPaneeli.contents.clear()
        peliIkkuna.size = new Dimension(300, 100)
        peliPaneeli.contents += new TextField("Odotetaan muita pelaajia. . .", 20) {
        editable = false
        }
      }
    })
  }
  
  
  
  private var peliSäie: Thread = null
  private def aloita(): Unit = {
    peliSäie = new Thread(new PeliSäie)
    peliSäie.start()
  }
  
  class PeliSäie extends Runnable {
    def run = {
      new Peli(nimiLista, tekoälyjä)
    }
  }
  
  
  private var ilmoitettu = false
  
  // Erillinen metodi pääsäikeen valintaan.
  def voittoIlmoitus(v: Option[Pelaaja]) = {
    SwingUtilities.invokeLater(new Runnable() {
      def run() = {    
        säikeelläIlmoitus(v)
      }
    })
  }
  
  
  private def säikeelläIlmoitus(voittaja: Option[Pelaaja]) = {
    if (!ilmoitettu) {
      peliPaneeli.contents.clear()
      peliIkkuna.size = new Dimension(200, 150)
      val teksti = {
        if (voittaja == None) "Tasapeli. (Kaikki oli huonoja.)" 
        else voittaja.get.toString + " voittaa!"
      }
      peliPaneeli.contents += new TextField(teksti, 25) {
        editable = false
        focusable = false
        text = teksti
      }
      val napit = new BoxPanel(Orientation.Horizontal)
      napit.contents += Button("Alusta") {
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


