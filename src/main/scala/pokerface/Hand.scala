package pokerface

import scala.collection.concurrent.RDCSS_Descriptor

/**
 * Created by Eliah on 9/6/2014.
 */
class Hand(val cards: Set[Card]) {

  def hasFullHouse: Boolean = {
    val groupedByRank = cards.groupBy(_.rank)
    val hasPair = groupedByRank.exists(_._2.size == 2)
    val hasThreeOfAKind = groupedByRank.exists(_._2.size == 3)

    return hasPair && hasThreeOfAKind
  }

  def hasJacksOrBetterPair: Boolean = {
    val groupedByRank = cards.groupBy(_.rank)
    return groupedByRank.exists(g => g._2.size == 2 && g._1 >= 11)
  }

  def getType = {
    if (hasFullHouse) {
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

