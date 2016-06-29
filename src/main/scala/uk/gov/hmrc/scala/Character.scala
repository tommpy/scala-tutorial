package uk.gov.hmrc.scala

trait Character {
  val firstName: String
  val house: Option[String]
}

object Character {
  def apply(name: String): ConcreteCharacter = {
    val split = name.split(' ')
    ConcreteCharacter(split.head, split.tail.lastOption)
  }
}

case class ConcreteCharacter(firstName: String, house: Option[String]) extends Character {
  override def toString: String = firstName + house.map(" " + _).getOrElse("")
}

abstract class Event
case class Killed(name: String) extends Event
case class KilledBy(name: String) extends Event
case class RessurrectedBy(name: String) extends Event

case class Biography(events: Seq[Event])