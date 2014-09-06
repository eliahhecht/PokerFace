package pokerface

import org.scalatest.FlatSpec

/**
 * Created by Eliah on 9/6/2014.
 */
class CardTestSpec extends FlatSpec {
  behavior of "pokerface.Card"

  it should "be able to have rank 1" in {
    new Card(Suit.Clubs, 1)
  }

  it should "throw if passed rank 0" in {
    intercept[Exception] {
      new Card(Suit.Clubs, 0)
    }
  }

  it should "throw if passed a negative rank" in {
    intercept[Exception] {
      new Card(Suit.Clubs, -1)
    }
  }

  it should "throw if passed a rank greater than 13" in {
    intercept[Exception] {
      new Card(Suit.Clubs, 14)
    }
  }

  it should "parse 1C as one of clubs" in {
    val card = Card.parse("1C").get
    assert(card.suit == Suit.Clubs)
    assert(card.rank == 1)
  }

  it should "parse 10H as ten of hearts" in {
    val card = Card.parse("10H").get
    assert(card.suit == Suit.Hearts)
    assert(card.rank == 10)
  }

  it should "fail to parse 1X" in {
    val card = Card.parse("1X")
    assert(card.isEmpty)
  }

  it should "parse QD as queen of diamonds" in {
    val card = Card.parse("QD").get
    assert(card.suit == Suit.Diamonds)
    assert(card.rank == 12)
  }

  it should "parse ks as spades" in {
    val card = Card.parse("ks").get
    assert(card.suit == Suit.Spades)
  }

  it should "parse ks as king" in {
    val card = Card.parse("ks").get
    assert(card.rank == 13)
  }

  it should "parse Ks as king" in {
    val card = Card.parse("Ks").get
    assert(card.rank == 13)
  }

  it should "parse kS as spades" in {
    val card = Card.parse("kS").get
    assert(card.suit == Suit.Spades)
  }

  it should "parse jd as jack" in {
    val card = Card.parse("jd").get
    assert(card.rank == 11)
  }

  it should "parse 99d as none" in {
    val card = Card.parse("99d")
    assert(card.isEmpty)
  }

  it should "parse foo as none" in {
    val card = Card.parse("foo")
    assert(card.isEmpty)
  }

  it should "parse A as Ace" in {
    val card = Card.parse("ad").get
    assert(card.rank == 1)
  }

  it should "toString 9s as 9S" in {
    val card = Card.parse("9s").get
    assert(card.toString == "9S")
  }

  it should "toString KD as KD" in {
    val card = Card.parse("kd").get
    assert(card.toString == "KD")
  }

  it should "compare equal cards as equal" in {
    val card1 = Card.parse("1d").get
    val card2 = Card.parse("1d").get

    assert(card1 == card2)
  }

}
