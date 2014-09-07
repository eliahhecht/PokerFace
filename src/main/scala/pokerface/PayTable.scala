package pokerface

/**
 * Created by Eliah on 9/6/2014.
 */
object PayTable {

  val RoyalFlush: Int = 5000
  val StraightFlush: Int = 1500
  val FourOfAKind: Int = 600
  val FullHouse: Int = 300
  val Flush: Int = 200
  val Straight: Int = 125
  val ThreeOfAKind: Int = 75
  val TwoPair: Int = 40
  val JacksOrBetter: Int = 10

  def getValue(hand: Hand): Int =
    hand.getRank match {
      case HandType.RoyalFlush => RoyalFlush
      case HandType.StraightFlush => StraightFlush
      case HandType.FourOfAKind => FourOfAKind
      case HandType.FullHouse => FullHouse
      case HandType.Flush => Flush
      case HandType.Straight => Straight
      case HandType.ThreeOfAKind => ThreeOfAKind
      case HandType.TwoPair => TwoPair
      case HandType.JacksOrBetter => JacksOrBetter
      case HandType.Nothing => 0
    }
}
