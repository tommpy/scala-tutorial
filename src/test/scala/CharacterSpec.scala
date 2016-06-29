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
                           "Arya", "Sandor", "Joffrey", "Renly", "Robert", "Stannis", "Catelyn", "Tywin", "Roose", "Ramsay", "Obara", "Gregor",
                           "Varys", "Mace", "Olenna", "Benjen")
      getFirstNames(characters) mustBe firstNames
    }

    "get all the houses" in {
      val houses = Seq("Lannister", "Targaryen", "Snow", "Baelish", "Mormont",
                        "Stark", "Clegane", "Baratheon", "Bolton", "Sand", "Tyrell")

      getHouses(characters) mustBe houses
    }

    "Find that Jon Snow is a bastard" in {
      isBastard(Character("Jon Snow")) mustBe true
    }

    "Find that Obara Sand is a bastard" in {
      isBastard(Character("Obara Sand")) mustBe true
    }

    "Find that Ned Stark is not a bastard" in {
      isBastard(Character("Ned Stark")) mustBe false
    }

    "Locate all the bastards in a list" in {
      findMeTheBastards(characters) mustBe Seq(Character("Jon Snow"), Character("Ramsay Snow"), Character("Obara Sand"))
    }
  }
}
