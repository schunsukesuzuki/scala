object Europe extends CurrencyZone {
  abstract class Euro extends AbstractCurrency {
    def desination = "EUR"
  }
  type Curency = Euro
  def make (cents: Long) = new Euro {
    val amount = cents
  }
  val Cent = make(1)
  val Euro make(100)
  val CurrencyUnit = Euro
}

Object Japan extends CurrencyZone {
  abstract class Yen extends AbstractCurrency {
    def designation = "JPY"
  }
  type Currency = Yen
  def make (yen :Long) = new Yen {
    val amount = yen
  }
  val Yen = make(1)
  val CurrencyUnitt = Yen
}