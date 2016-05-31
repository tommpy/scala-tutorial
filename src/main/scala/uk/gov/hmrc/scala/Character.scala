package uk.gov.hmrc.scala

trait Character {
  val firstName: String
  val house: Option[String]
}

object Character {
  def apply(name: String): Character = {
    val split = name.split(' ')
    ConcreteCharacter(split.head, split.tail.lastOption)
  }
}

case class ConcreteCharacter(firstName: String, house: Option[String]) extends Character
