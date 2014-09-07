package pokerface

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Eliah on 9/6/2014.
 */
class HandSpec extends FlatSpec with Matchers {

  behavior of "pokerface.Hand"

  it should "be able to hold five cards" in {
    val cards = GetCards(5)
    new Hand(cards)
  }

  it should "throw if passed more than 5 cards" in {
    val cards = GetCards(6)
    intercept[Exception] {
      new Hand(cards)
    }
  }

  it should "throw if passed fewer than 5 cards" in {
    val cards = GetCards(1)
    intercept[Exception] {
      new Hand(cards)
    }
  }

  def GetCards(numberOfCards: Int): Seq[Card] = {
    val cards = (1 to numberOfCards).map(n => new Card(Suit.Diamonds, n)).toSeq
    cards
  }

  it should "parse '1d 2d 3d 4d 5d' correctly" in {
    val hand = Hand.parse("1d 2d 3d 4d 5d").get
    val expected = new Hand(GetCards(5))
    assert(hand == expected)
  }

  it should "parse '2s 3h kd 7d as' correctly" in {
    val hand = Hand.parse("2s 3h kd 7d as").get
    val expected = new Hand(Seq(Card.parse("2s").get,
      Card.parse("3h").get,
      Card.parse("kd").get,
      Card.parse("7d").get,
      Card.parse("as").get))

    assert(hand == expected)
  }

  it should "return none on unparseable hand" in {
    val hand = Hand.parse("foo bar baz")
    assert(hand.isEmpty)
  }

  it should "trim before parsing" in {
    val hand = Hand.parse(" 2s 3d 4s 5d 6s ")
    assert(hand.isDefined)
  }

  it should "parse an empty string as an empty set of cards" in {
    val hand = CardSetParser.parse("")
    hand shouldBe empty
  }
}
