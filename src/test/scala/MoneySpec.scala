import org.scalatest.{WordSpec, MustMatchers}
import uk.gov.hmrc.scala._

class MoneySpec extends WordSpec with MustMatchers {
  import MoneyHelpers._

  "Checkout" should {
    "Give back 10 pennies" in {
      Checkout.buy(CopperPennies(10), CopperPennies(20)) mustBe Coins(0, 0, 10)
    }
  }

  "Penny converter" should {
    "convert stags to pennies" in {
      stagConverter.toPennies(SilverStags(1)) mustBe CopperPennies(56)
    }

    "convert pennies to stags" in {
      stagConverter.toCurrency(CopperPennies(56)) mustBe SilverStags(1)
    }

    "convert dragons to pennies" in {
      dragonConverter.toPennies(GoldenDragons(1))  mustBe CopperPennies(56 * 210)
    }

    "convert pennies to dragons" in {
      dragonConverter.toCurrency(CopperPennies(56 * 210)) mustBe GoldenDragons(1)
    }

    "convert pennies to pennies" in {
      pennyConverter.toPennies(CopperPennies(1)) mustBe CopperPennies(1)
      pennyConverter.toCurrency(CopperPennies(1)) mustBe CopperPennies(1)
    }

    "convert coins to pennies" in {
      val pennies = coinConverter.toPennies(Coins(1, 0, 0))
      pennies mustBe CopperPennies(210 * 56)
    }

    "convert pennies to coins" in {
      val coins = coinConverter.toCurrency(CopperPennies(210 * 56))
      coins mustBe Coins(1, 0, 0)
    }
  }
}
