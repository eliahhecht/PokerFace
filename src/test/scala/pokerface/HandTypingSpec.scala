package pokerface

import org.scalatest.FlatSpec

/**
 * Created by Eliah on 9/6/2014.
 */
class HandTypingSpec extends FlatSpec{

  behavior of "pokerface.Hand"

  it should "score a jacks-or-better pair as jacks or better" in {
    val hand = Hand.parse("jd js 1d 2c 3s").get
  }

}
