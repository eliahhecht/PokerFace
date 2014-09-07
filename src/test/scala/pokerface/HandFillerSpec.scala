package pokerface

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Eliah on 9/6/2014.
 */
class HandFillerSpec extends FlatSpec with Matchers {

  behavior of  "HandFiller"

  it should "not add anything to a five-card hand" in {
    val hand = sampleHand
    val keeps = sampleHand
    val fills = new HandFiller().AllFills(hand, keeps)

    fills.size shouldEqual 1
  }

  val sampleHand = Hand.parse("1d 2d 3d 4d 5d").get

  it should "add each possible card to a four-card hand" in {
    val keeps = CardSetParser.parse("1d 2d 3d 4d")
    val fills = new HandFiller().AllFills(sampleHand, keeps)

    fills.size shouldEqual 47
  }

  it should "add each possible card to a three-card hand" in {
    val keeps = CardSetParser.parse("1d 2d 3d")
    val fills = new HandFiller().AllFills(sampleHand, keeps)

    fills.size shouldEqual 1081
  }

  it should "add each possible card to a two-card hand" in {
    val keeps = CardSetParser.parse("1d 2d")
    val fills = new HandFiller().AllFills(sampleHand, keeps)

    fills.size shouldEqual 16215
  }

  it should "add each possible card to a one-card hand" in {
    val keeps = CardSetParser.parse("1d")
    val fills = new HandFiller().AllFills(sampleHand, keeps)

    fills.size shouldEqual 178365
  }

}
