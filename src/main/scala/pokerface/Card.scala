package pokerface

import pokerface.Suit.Suit

/**
 * Created by Eliah on 9/6/2014.
 */
case class Card(val suit: Suit, val rank: Int) {

  if (rank < 1 || rank > 13) {
    throw new IllegalArgumentException("Rank must be between 1 and 13")
  }

  override def toString =  {
    val rankString = rank match {
      case 11 => "J"
      case 12 => "Q"
      case 13 => "K"
      case 1 => "A"
      case r => r
    }

    rankString + suit.toString
  }

}


object Card {
  def parse(s: String) = {
    val normalized = s.toUpperCase

    val suit = ParseSuit(normalized)

    val rank = parseRank(normalized)

    if (suit.isDefined && rank.isDefined)
    {
      Some(new Card(suit.get, rank.get))
    }
    else {
      None
    }
  }

  def parseRank(normalized: String): Option[Int] = {
    val rank = normalized match {
      case r if r.startsWith("J") => Some(11)
      case r if r.startsWith("Q") => Some(12)
      case r if r.startsWith("K") => Some(13)
      case r if r.startsWith("A") => Some(1)
      case _ if normalized.matches("\\d+.") => Some(normalized.dropRight(1).toInt)
      case _ => None
    }

    if (rank.isDefined) {
      if (rank.get < 1 || rank.get > 13) {
        return None
      }
    }

    rank
  }

  def ParseSuit(normalized: String): Option[Suit.Value] = {
    normalized.last match {
      case 'C' => Some(Suit.Clubs)
      case 'D' => Some(Suit.Diamonds)
      case 'H' => Some(Suit.Hearts)
      case 'S' => Some(Suit.Spades)
      case _ => None
    }
  }
}
