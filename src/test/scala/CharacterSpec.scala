import org.scalatest.{WordSpec, MustMatchers}
import uk.gov.hmrc.scala.{CharacterUtils, Character, ConcreteCharacter}

class CharacterSpec extends WordSpec with MustMatchers {
  import CharacterFixtures._
  import CharacterUtils._

  "Character" should {
    "give the name" in {
      Character("Tyrion Lannister").firstName mustBe "Tyrion"
    }

    "work with equality" in {
      new ConcreteCharacter("Tyrion", Some("Lannister")) must equal(new ConcreteCharacter("Tyrion", Some("Lannister")))
    }
  }

  "Fixtures" should {
    "have varys in" in {
      characters.find(_.firstName == "Varys") mustBe Some(ConcreteCharacter("Varys", None))
    }

    "not have Brienne of Tarth in" in {
      characters.find(_.firstName == "Brienne") mustBe None
    }
  }
  
  "Name Extractor" should {
    "get all the first names" in {
      val firstNames = Seq("Tyrion", "Jaime", "Cersei", "Daenerys", "Jon", "Petyr", "Jorah", "Sansa",
                           "Arya", "Sandor", "Joffrey", "Catelyn", "Tywin", "Roose", "Sandor", "Gregor",
                           "Varys")
      getFirstNames(characters) mustBe firstNames
    }

    "get all the houses" in {
      val houses = Seq("Lannister", "Targaryen", "Snow", "Baelish", "Mormont",
                        "Stark", "Clegane", "Baratheon", "Bolton")

      getHouses(characters) mustBe houses
    }
  }
}
