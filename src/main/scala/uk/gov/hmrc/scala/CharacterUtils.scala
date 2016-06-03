package uk.gov.hmrc.scala

object CharacterUtils {
  def getFirstNames(characters: Seq[Character]): Seq[String] = characters.map(_.firstName)
  def getHouses(characters: Seq[Character]): Seq[String] = characters.flatMap(_.house).distinct

  private val bastardNames = Seq("Snow", "Sand", "Rivers", "Stone")
  def isBastard(character: Character): Boolean = character.house.exists(n => bastardNames.contains(n))

  def findMeTheBastards(characters: Seq[Character]): Seq[Character] = characters.filter(isBastard)
}
