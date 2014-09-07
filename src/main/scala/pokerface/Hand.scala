package pokerface

import scala.collection.immutable.HashSet

/**
 * Created by Eliah on 9/6/2014.
 */
class Hand(val cards: Seq[Card]) extends Seq[Card] {

  private def hasFullHouse: Boolean = {
    val hasPair = groupedByRank.exists(_._2.size == 2)
    val hasThreeOfAKind = groupedByRank.exists(_._2.size == 3)

    hasPair && hasThreeOfAKind
  }

  private val rankMask: Long = cards.map(c => math.pow(2, c.rank - 1).toInt).foldLeft(0){_ | _}

  private val straights: HashSet[Long] = HashSet(
    0x1F00,
    0xF80,
    0x7C0,
    0x3E0,
    0xF8,
    0x7C,
    0x3E,
    0x1F,
    0x1E01
  )

  private def hasJacksOrBetterPair: Boolean = {
    groupedByRank.exists(g => g._2.size == 2 && (g._1 >= 11 || g._1 == 1))
  }

  private def hasFourOfAKind: Boolean = {
    groupedByRank.exists(_._2.size == 4)
  }

  private lazy val groupedByRank = cards.groupBy(_.rank)

  private def hasStraight: Boolean = {
    return straights.contains(rankMask)
  }

  private def hasStraightFlush: Boolean = {
    hasStraight && hasFlush
  }

  private def hasRoyalFlush: Boolean = {
    hasStraightFlush && cards.exists(_.rank == 13) && cards.exists(_.rank == 1)
  }

  private def hasFlush: Boolean = {
    cards.groupBy(_.suit).size == 1
  }

  private def hasThreeOfAKind: Boolean = {
    groupedByRank.exists(_._2.size == 3)
  }

  private def hasTwoPairs: Boolean = {
    groupedByRank.count(_._2.size == 2) == 2
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

  override def toString() = cards.mkString(",")

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Hand]) {
      false
    } else {
      val hands = obj.asInstanceOf[Hand]
      hands.cards.equals(this.cards)
    }
  }

  override def length: Int = cards.length

  override def apply(idx: Int): Card = cards(idx)

  override def iterator: Iterator[Card] = cards.iterator
}

object Hand {

  def parse(s: String): Option[Hand] = {
    val cards = CardSetParser.parse(s)
    if (cards.length == 5) {
      Some(new Hand(cards))
    }
    else {
      None
    }
  }

}

object CardSetParser {
  def parse(s: String): Seq[Card] = s.trim.split("\\s+").flatMap(Card.parse)
}


object HandType extends Enumeration {
  type HandType = Value
  val Nothing, JacksOrBetter, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush, RoyalFlush
  = Value
}

