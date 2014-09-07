package pokerface

import scala.collection.concurrent.RDCSS_Descriptor

/**
 * Created by Eliah on 9/6/2014.
 */
class Hand(val cards: Set[Card]) {

  private def hasFullHouse: Boolean = {
    val hasPair = groupedByRank.exists(_._2.size == 2)
    val hasThreeOfAKind = groupedByRank.exists(_._2.size == 3)

    return hasPair && hasThreeOfAKind
  }

  private def hasJacksOrBetterPair: Boolean = {
    return groupedByRank.exists(g => g._2.size == 2 && g._1 >= 11)
  }

  private def hasFourOfAKind: Boolean = {
    return groupedByRank.exists(_._2.size == 4)
  }

  private def groupedByRank = cards.groupBy(_.rank)

  private def hasAceHighStraight(sorted: Seq[Card]): Boolean = {
    return (sorted.map(_.rank).equals(Seq(1, 10, 11, 12, 13)))
  }

  private def hasStraight: Boolean = {
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

  private def hasStraightFlush: Boolean = {
    return hasStraight && hasFlush
  }

  private def hasRoyalFlush: Boolean = {
    return hasStraightFlush && cards.exists(_.rank == 13) && cards.exists(_.rank == 1)
  }

  private def hasFlush: Boolean = {
    return cards.groupBy(_.suit).size == 1
  }

  private def hasThreeOfAKind: Boolean = {
    return groupedByRank.exists(_._2.size == 3)
  }

  private def hasTwoPairs: Boolean = {
    return groupedByRank.count(_._2.size == 2) == 2
  }

  def getRank = {
    if (hasRoyalFlush) {
      HandType.RoyalFlush
    }
    else if (hasStraightFlush) {
      HandType.StraightFlush
    }
    else if (hasFlush) {
      HandType.Flush
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
    else if (hasThreeOfAKind) {
      HandType.ThreeOfAKind
    }
    else if (hasTwoPairs) {
      HandType.TwoPair
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

