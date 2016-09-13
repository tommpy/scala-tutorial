package uk.gov.hmrc.scala

sealed trait Currency
case class GoldenDragons(n: Int) extends Currency
case class SilverStags(n: Int) extends Currency
case class CopperPennies(n: Int) extends Currency {
  def -(other: CopperPennies) = CopperPennies(n - other.n)
}

case class Coins(dragons: GoldenDragons, silverStags: SilverStags, pennies: CopperPennies)

trait PennyConverter[C] {
  def toPennies(currency: C): CopperPennies
  def toCurrency(pennies: CopperPennies): C
}

object MoneyHelpers {
  implicit val dragonConverter = new PennyConverter[GoldenDragons] {
    override def toPennies(currency: GoldenDragons): CopperPennies = {
      CopperPennies(currency.n * 56 * 210)
    }

    override def toCurrency(pennies: CopperPennies): GoldenDragons = {
      GoldenDragons(pennies.n / (56 * 210))
    }
  }

  implicit val stagConverter = new PennyConverter[SilverStags] {
    override def toPennies(currency: SilverStags): CopperPennies = {
      CopperPennies(currency.n * 56)
    }

    override def toCurrency(pennies: CopperPennies): SilverStags = {
      SilverStags(pennies.n / 56)
    }
  }

  implicit val pennyConverter = new PennyConverter[CopperPennies] {
    override def toPennies(currency: CopperPennies): CopperPennies = {
      CopperPennies(currency.n)
    }

    override def toCurrency(pennies: CopperPennies): CopperPennies = {
      CopperPennies(pennies.n)
    }
  }

  implicit val coinConverter = new PennyConverter[Coins] {
    override def toPennies(currency: Coins): CopperPennies = {
      val total = currency.silverStags.as[CopperPennies].n +
      currency.dragons.as[CopperPennies].n + currency.pennies.n

      CopperPennies(total)
    }

    override def toCurrency(pennies: CopperPennies): Coins = {
      val dragons = pennies.as[GoldenDragons]
      val remainder = pennies - dragons.as[CopperPennies]
      val stags = remainder.as[SilverStags]
      val coppers = remainder - stags.as[CopperPennies]

      Coins(dragons, stags, coppers)
    }
  }

  implicit class CurrencyHelpers[C: PennyConverter](currency: C) {
    def as[C1: PennyConverter]: C1 = {
      val p = implicitly[PennyConverter[C]].toPennies(currency)
      implicitly[PennyConverter[C1]].toCurrency(p)
    }

    def -[C1: PennyConverter](other: C1): Coins = {
      val converter1 = implicitly[PennyConverter[C]]
      val converter2 = implicitly[PennyConverter[C1]]
      val pennies = converter1.toPennies(currency) - converter2.toPennies(other)
      pennies.as[Coins]
    }
  }

  implicit def intAsDragon(i: Int) = GoldenDragons(i)
  implicit def intAsStag(i: Int) = SilverStags(i)
  implicit def intAsPenny(i: Int) = CopperPennies(i)
}

object Checkout {
  import MoneyHelpers._

  val s = Seq(1, 2, 3)
  s.sorted

  def buy[C: PennyConverter](cost: C, tendered: C): Coins = {
    tendered - cost
  }
}
