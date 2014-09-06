package pokerface

import scala.collection.concurrent.RDCSS_Descriptor

/**
 * Created by Eliah on 9/6/2014.
 */
class Hand(val cards: Set[Card]) {

  def hasFullHouse: Boolean = {
    val hasPair = groupedByRank.exists(_._2.size == 2)
    val hasThreeOfAKind = groupedByRank.exists(_._2.size == 3)

    return hasPair && hasThreeOfAKind
  }

  def hasJacksOrBetterPair: Boolean = {
    return groupedByRank.exists(g => g._2.size == 2 && g._1 >= 11)
  }

  def hasFourOfAKind: Boolean = {
    return groupedByRank.exists(_._2.size == 4)
  }

  def groupedByRank = cards.groupBy(_.rank)

  def hasAceHighStraight(sorted: Seq[Card]): Boolean = {
    return (sorted.map(_.rank).equals(Seq(1, 10, 11, 12, 13)))
  }

  def hasStraight: Boolean = {
    val sorted = cards.toList.sortBy(_.rank)
    if (hasAceHighStraight(sorted)) {
      return true
    }
    for (cardIdx <- 1 to 4) {
      if (sorted(cardIdx).rank - sorted(cardIdx - 1).rank != 1) {
        return false
      }
    }
    return true
  }

  def hasStraightFlush: Boolean = {
    val groupedBySuit = cards.groupBy(_.suit)
    return hasStraight && groupedBySuit.size == 1
  }

  def getRank = {
    if (hasStraightFlush) {
      HandType.StraightFlush
    }
    else if (hasFourOfAKind) {
      HandType.FourOfAKind
    }
    else if (hasStraight) {
      HandType.Straight
    }
    else if (hasFullHouse) {
      HandType.FullHouse
    }
    else if (hasJacksOrBetterPair) {
      HandType.JacksOrBetter
    }
    else {
      HandType.Nothing
    }
  }

  if (cards.size != 5) {
    throw new IllegalArgumentException("Number of cards must be exactly 5")
  }

  override def toString = cards.mkString(",")

  override def equals(obj: Any): Boolean = {
    if (!(obj.isInstanceOf[Hand])) {
      return false
    } else {
      val hands = obj.asInstanceOf[Hand]
      return hands.cards.equals(this.cards)
    }
  }

}

object Hand {

  def parse(s: String): Option[Hand] = {
    val cards = s.split(" ").flatMap(Card.parse(_))
    if (cards.length == 5) {
      Some(new Hand(cards.toSet))
    }
    else {
      None
    }
  }

}


object HandType extends Enumeration {
  type HandType = Value
  val Nothing, JacksOrBetter, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush, RoyalFlush
  = Value
}

