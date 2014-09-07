package pokerface


/**
 * Created by Eliah on 9/6/2014.
 */
class HandFiller {

  def AllFills(hand: Hand, keeps: Seq[Card]): Seq[Hand] = {
    val restOfDeck = new Deck().except(hand.cards)
    val spotsToFill = 5 - keeps.size
    if (spotsToFill == 0) {
      return Seq(new Hand(keeps))
    } else {
      return restOfDeck.getCards.combinations(spotsToFill)
        .map(fill => new Hand(keeps ++ fill))
        .toSeq
    }
  }

  def PartialFills(hand: Hand, spots: Int) : Seq[Seq[Card]] = {
    val restOfDeck = new Deck().except(hand.cards)
    if (spots == 0) {
      return Seq(Seq.empty)
    } else {
      return restOfDeck.getCards.combinations(spots).toSeq
    }
  }
}

class Deck extends Seq[Card] {
  var cards = for {rank <- 1 to 13
                   suit <- List(Suit.Clubs, Suit.Diamonds, Suit.Hearts, Suit.Spades)}
  yield new Card(suit, rank)

  def getCards = cards

  def except(cardsToRemove: Seq[Card]): Deck = {
    val d = new Deck()
    d.cards = for {card <- cards
                   if !cardsToRemove.contains(card)
    } yield card
    d
  }

  override def length: Int = cards.length

  override def apply(idx: Int): Card = cards(idx)

  override def iterator: Iterator[Card] = cards.iterator
}

