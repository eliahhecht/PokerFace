package pokerface

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}
import pokerface.HandType.HandType

/**
 * Created by Eliah on 9/6/2014.
 */
class HandTypingSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks{

  behavior of "pokerface.Hand"

  val hands = Table(
    ("hand", "expected rank"),
    ("jd js 1d 2c 3s", HandType.JacksOrBetter),
    ("qd qs 1d 2c 3s", HandType.JacksOrBetter),
    ("qd js 1d 2c 3s", HandType.Nothing),
    ("1d 3c 5s 7d jd", HandType.Nothing),
    ("1d 3c 3s 7d jd", HandType.Nothing),
    ("ac ah ad 3d 3s", HandType.FullHouse),
    ("ac ad ah as jd", HandType.FourOfAKind),
    ("1d 2c 3d 4d 5d", HandType.Straight),
    ("10s jd qd ks as", HandType.Straight),
    ("kd ac 1d 2d 3d", HandType.Nothing),
    ("3d 3s 5d 6d 7d", HandType.Nothing),
    ("10d jd qd kd ad", HandType.RoyalFlush),
    ("1d 3d 5d 7d 9d", HandType.Flush),
    ("9s 10s js qs ks", HandType.StraightFlush)

  )

  it should "rank hands correctly" in forAll(hands) {(handString: String, expectedRank: HandType) =>
    val hand = Hand.parse(handString)
    assume(hand.isDefined)
    hand.get.getRank shouldEqual expectedRank
  }
}
