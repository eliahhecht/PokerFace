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
    ("ac ah ad 3d 3s", HandType.FullHouse)
  )

  it should "rank hands correctly" in forAll(hands) {(handString: String, expectedRank: HandType) =>
    val hand = Hand.parse(handString)
    assume(hand.isDefined)
    hand.get.getType shouldEqual expectedRank
  }
}
