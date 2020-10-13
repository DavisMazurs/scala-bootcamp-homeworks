package adt

import adt.EitherConverter.EitherList
import adt.AlgebraicDataTypes.GameMode._

object AlgebraicDataTypes {

  type ErrorMessage = String

  sealed trait GameMode
  object GameMode {
    final case object TexasHoldem extends GameMode
    final case object OmahaHoldem extends GameMode
  }

  final case class Rank private (value: Int) extends AnyVal
  object Rank {
    def create(value: Char): Either[ErrorMessage, Rank] = value match {
      case '2' => Right(Rank(2))
      case '3' => Right(Rank(3))
      case '4' => Right(Rank(4))
      case '5' => Right(Rank(5))
      case '6' => Right(Rank(6))
      case '7' => Right(Rank(7))
      case '8' => Right(Rank(8))
      case '9' => Right(Rank(9))
      case 'T' => Right(Rank(10))
      case 'J' => Right(Rank(11))
      case 'Q' => Right(Rank(12))
      case 'K' => Right(Rank(13))
      case 'A' => Right(Rank(14))
      case _ => Left(s"Invalid rank entered '$value'")
    }
  }

  //Suits
  final case class Suit private (value: Char) extends AnyVal
  object Suit {
    def create(value: Char): Either[ErrorMessage, Suit] = value match {
      case 'c' | 'd' | 'h' | 's' => Right(Suit(value))
      case _ => Left(s"Invalid suit entered '$value'")
    }
  }

  final case class Card private (rank: Rank, suit: Suit)
  object Card {
    def create(input: String): Either[ErrorMessage, Card] = input.toList match {
      case c1 :: c2 :: Nil => for {
        rank <- Rank.create(c1)
        suit <- Suit.create(c2)
      } yield Card(rank, suit)
      case _ => Left(s"Invalid input for card '$input'")
    }
  }


  final case class Hand private (cards: List[Card])
  object Hand {
    def create(cards: List[Card], gameMode: GameMode): Either[ErrorMessage, Hand] = gameMode match {
      case TexasHoldem if cards.length == 2 => Right(Hand(cards))
      case OmahaHoldem if cards.length == 4 => Right(Hand(cards))
      case _ => Left("Invalid card count for hand")
    }
  }

  final case class Board private (cards: List[Card])
  object Board {
    def create(cards: List[Card]): Either[ErrorMessage, Board] = cards.size match {
      case 5 => Right(Board(cards))
      case _ => Left("Invalid card count for board")
    }
    //Alternative
    def createFromString(input: String): Either[ErrorMessage, Board] = input match {
      case i if i.length == 10 =>
        (for {
          str <- i.toSeq.sliding(2, 2).map(_.unwrap).toList
        } yield Card.create(str)).toEitherList match {
          case Left(x) => Left(x)
          case Right(x) => Right(Board(x))
        }
      case _ => Left("Invalid card count for board")
    }
  }

  type Result = List[List[Hand]]

  object GameInstance {
    def create(input: String, gameMode: GameMode = GameMode.TexasHoldem): String = {
      val cardSets = input.split(" ")
      val board = (for {
        str <- cardSets.head.toSeq.sliding(2, 2).map(_.unwrap).toList
      } yield Card.create(str)).toEitherList match {
        case Right(x) => Board.create(x)
        case Left(x) => Left(x)
      }

      val hands = (for {
        seq <- cardSets.tail.toList
        str <- seq.toSeq.sliding(2, 2).map(_.unwrap).toList
      } yield Card.create(str)).toEitherList match {
        case Right(x) => (for {
          cards <- x.sliding(2, 2).toList
        } yield Hand.create(cards, gameMode)).toEitherList
        case Left(x) => Left(x)
      }

      (for {
        b <- board
        h <- hands
      } yield process(b, h)) match {
        case Right(x) => x match {
          case Right(value) => value.toString()
          case Left(err) => err
        }
        case Left(x) => x
      }
    }
  }

  //Just sorts hands by rank, as example
  def process(board: Board, hands: List[Hand]): Either[ErrorMessage, Result] = hands match {
    case x if x.nonEmpty =>
      Right(List(x.sortBy(h => h.cards.head.rank.value)))
    case _ => Left("Processing error")
  }

  sealed trait Combo
  object Combo {
    final case object HighCard extends Combo
    final case object Pair extends Combo
    final case object TwoPairs extends Combo
    final case object ThreeOfAKind extends Combo
    final case object Straight extends Combo
    final case object Flush extends Combo
    final case object FullHouse extends Combo
    final case object FourOfAKind extends Combo
    final case object StraightFlush extends Combo
  }

  def main(args: Array[String]): Unit = {
    println(GameInstance.create("4c5h7d9s3s JcKc QcAc 2c3c 4h5h 7s8s"))
  }
}

object EitherConverter {
  implicit class EitherList[E, T](list: List[Either[E, T]]) {
    def toEitherList: Either[E, List[T]] = {
      def convert(eithers: List[Either[E, T]], list: List[T]): Either[E, List[T]] = eithers match {
        case Nil => Right(list)
        case x :: xs => x match {
          case Left(err) => Left(err)
          case Right(el) => convert(xs, el :: list)
        }
      }
      convert(list, Nil)
    }
  }
}
