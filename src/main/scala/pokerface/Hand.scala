package pokerface

/**
 * Created by Eliah on 9/6/2014.
 */
class Hand(cards: Set[Card]) {

  if (cards.size != 5) {
    throw new IllegalArgumentException("Number of cards must be exactly 5")
  }


}


object HandType extends Enumeration {
  type HandType = Value
  val JacksOrBetter, TwoPair, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush, RoyalFlush = Value
}

