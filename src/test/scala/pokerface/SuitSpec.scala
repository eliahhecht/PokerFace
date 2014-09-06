package pokerface

import org.scalatest.FlatSpec

/**
 * Created by Eliah on 9/6/2014.
 */
class SuitSpec extends FlatSpec {

  "suit" should "tostring using its first letter" in {
    assert(Suit.Diamonds.toString == "D")
  }

}
