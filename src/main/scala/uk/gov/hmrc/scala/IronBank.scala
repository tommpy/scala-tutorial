package uk.gov.hmrc.scala

import scala.util.{Success, Failure, Try}

trait DeathRegister {
  def isDead(character: Character): Try[Boolean]
}

case class NotSureException(character: Character) extends Exception(s"Not sure if ${character.firstName} is alive or dead")

class CitadelDeathRegister extends DeathRegister {
  val deadCharacters = Seq(Character("Catelyn Stark"), Character("Robert Baratheon"), Character("Renly Baratheon"))
  val notSure = Seq(Character("Benjen Stark"))
  override def isDead(character: Character): Try[Boolean] = {
    if(notSure.contains(character))
      Failure(new NotSureException(character))
    else
      Success(deadCharacters.contains(character))
  }
}

trait Transaction
case class Loan(amount: Long) extends Transaction
case class Repayment(amount: Long) extends Transaction

trait TransactionService {
  def previousTransactions: PartialFunction[Character, Seq[Transaction]]
}

trait HardcodedTransactionService extends TransactionService {
  def previousTransactions: PartialFunction[Character, Seq[Transaction]] = {
    case ConcreteCharacter("Tywin", Some("Lannister")) => Seq(Loan(1000), Loan(1000), Loan(1000), Loan(1000))
    case ConcreteCharacter("Varys", None) => Seq(Repayment(1000), Loan(1000))
  }
}

class IronBankBase(deathRegister: DeathRegister) {
  self: TransactionService =>

  type Result = Either[String, Long]

  val individual: PartialFunction[Character, Result] =  {
    case ConcreteCharacter("Stannis", Some("Baratheon")) => Right(10000)
    case ConcreteCharacter("Jon", Some("Snow")) => Right(9000)
  }

  val house: PartialFunction[Character, Result] = {
    case ConcreteCharacter(_, Some("Tyrell")) => Right(8000)
    case ConcreteCharacter(_, Some("Clegane" | "Bolton")) => Left("Probably going to kill you")
  }

  val dead: PartialFunction[Character, Result] = {
    case c @ ConcreteCharacter(firstName, _) => {
      deathRegister.isDead(c) match {
        case Success(false) => Right(200)
        case Success(true) => Left(s"$firstName is dead")
        case Failure(_) => Left("Not sure if they're alive")
      }
    }
  }

  val hasTransactions: PartialFunction[Seq[Transaction], Result] = {
    case Loan(_) :: Loan(_) :: rest => Left("Too many loans")
    case Repayment(_) :: rest => Right(4000)
  }

  val transactions =  previousTransactions.andThen(hasTransactions)

  val allPFs = Seq(individual, house, transactions, dead)
  val completeMatch = allPFs.reduce(_ orElse _)

  def approveLoan(character: Character): Result = {
    completeMatch.apply(character)
  }
}

object IronBank extends IronBankBase(new CitadelDeathRegister()) with HardcodedTransactionService
