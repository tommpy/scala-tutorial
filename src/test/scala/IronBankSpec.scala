import org.scalatest.{WordSpec, MustMatchers}
import uk.gov.hmrc.scala.{NotSureException, CitadelDeathRegister, IronBank}

import scala.util.Success

class IronBankSpec extends WordSpec with MustMatchers {
  import CharacterFixtures._
  /**
    * - Do:
    * - Give loans to Stannis and Jon
    * - Give a loan to a Tyrell
    * - Give a loan to anyone who has recently repaid
    * - Don't:
    * - Give a loan to a Clegane or a Bolton
    * - Give a loan to anyone dead
    * - Give a loan to anyone with more than 2 loans in a row
    * - Give a loan to anyone if we don't know whether they're alive
    */

  val approved = Seq(stannis -> 10000, jon -> 9000, mace -> 8000, varys -> 4000)
  val notApproved = Seq(
    gregor -> "Probably going to kill you",
    sandor -> "Probably going to kill you",
    roose -> "Probably going to kill you",
    robert -> "Robert is dead",
    benjen -> "Not sure if they're alive",
    tywin -> "Too many loans")

  "Iron Bank" should {
    approved.foreach { c =>
      s"Give a loan to ${c._1.firstName}" in {
        IronBank.approveLoan(c._1) mustBe Right(c._2)
      }
    }

    notApproved.foreach { c =>
      s"Not give a loan to ${c._1}" in {
        IronBank.approveLoan(c._1) mustBe Left(c._2)
      }
    }
  }

  "Citadel Death Register" should {
    val register = new CitadelDeathRegister()

    "show Catelyn, Robert and Renly as dead" in {
      register.isDead(catelyn) mustBe Success(true)
      register.isDead(renly) mustBe Success(true)
      register.isDead(robert) mustBe Success(true)
    }

    "show Varys as not dead" in {
      register.isDead(varys) mustBe Success(false)
    }

    "throw an exception if we're not sure if they're alive or dead" in {
      register.isDead(benjen) mustBe 'isFailure
    }
  }
}
