package pokerface

/**
 * Created by Eliah on 9/6/2014.
 */
class Hand(val cards: Set[Card]) {

  if (cards.size != 5) {
    throw new IllegalArgumentException("Number of cards must be exactly 5")
  }

  override def toString = cards.mkString(",")

  override def equals(obj: Any) : Boolean = {
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
    Some(new Hand((1 to 5).map(n => new Card(Suit.Diamonds, n)).toSet))
  }

}


object HandType extends Enumeration {
  type HandType = Value
  val JacksOrBetter, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush, RoyalFlush = Value
}

