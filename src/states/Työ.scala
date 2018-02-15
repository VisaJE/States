package states


class Työ(val kulutus: Vector[Tuote] = Vector(), val tuotto: Vector[Tuote] = Vector()) {
  
}


class Viljely extends Työ


class Nollatyö extends Työ