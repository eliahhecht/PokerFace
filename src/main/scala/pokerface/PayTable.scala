package pokerface

/**
 * Created by Eliah on 9/6/2014.
 */
object PayTable {

  def getValue(hand: Hand): Int =
    hand.getRank match {
      case HandType.RoyalFlush => 5000
      case HandType.StraightFlush => 1500
      case HandType.FourOfAKind => 600
      case HandType.FullHouse => 300
      case HandType.Flush => 200
      case HandType.Straight => 125
      case HandType.ThreeOfAKind => 75
      case HandType.TwoPair => 40
      case HandType.JacksOrBetter => 10
      case HandType.Nothing => 0
    }
}
