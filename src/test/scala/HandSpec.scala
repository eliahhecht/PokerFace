import org.scalatest.FlatSpec
import pokerface.{Suit, Card, Hand}

/**
 * Created by Eliah on 9/6/2014.
 */
class HandSpec extends FlatSpec {

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

  def GetCards(numberOfCards: Int): Set[Card] = {
    val cards = (1 to numberOfCards).map(n => new Card(Suit.Diamonds, n)).toSet
    cards
  }
}
