package pokerface

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Eliah on 9/6/2014.
 */
class SuggesterSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks{

  behavior of "pokerface.Suggester"

  val hands = Table(
    ("hand", "expected keeps"),
    ("5s kh jh 9h 9c", "9h 9c"),
    ("4s as 7h 6c 9d", "4s as"),
    ("10d jd qd kd ad", "10d jd qd kd ad"),
    ("10d jd qd kd 3s", "10d jd qd kd"),
    ("10d jd js kd 3s", "jd js"),
    ("jd 3d 5s 8h 9s", "jd"),
    ("2s 3d 6c 7s 9h", "")
  )

  it should "rank hands correctly" in forAll(hands) {(handString: String, expectedKeeps: String) =>
    val hand = Hand.parse(handString)
    assume(hand.isDefined)
    val suggestion = new Suggester().suggestKeeps(hand.get)

    val keeps = suggestion.keeps
    val ev = suggestion.expectedValue
    println(s"From $hand, keep $keeps worth $ev")

    keeps should contain theSameElementsAs CardSetParser.parse(expectedKeeps)
  }
}
