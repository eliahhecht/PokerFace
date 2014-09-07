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
    ("10d jd qd kd ad", "10d jd qd kd ad")
  )

  it should "rank hands correctly" in forAll(hands) {(handString: String, expectedKeeps: String) =>
    val hand = Hand.parse(handString)
    assume(hand.isDefined)
    val keeps = new Suggester().suggestKeeps(hand.get)

    keeps should contain theSameElementsAs CardSetParser.parse(expectedKeeps)
  }
}
