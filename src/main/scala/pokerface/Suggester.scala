package pokerface

import scala.collection.mutable

/**
 * Created by Eliah on 9/6/2014.
 */
class Suggester {

  def power[A](t: Seq[A]): Seq[Seq[A]] = {
    @annotation.tailrec
    def pwr(t: Seq[A], ps: Seq[Seq[A]]): Seq[Seq[A]] =
      if (t.isEmpty) ps
      else pwr(t.tail, ps ++ (ps map (seq => seq :+ t.head)))

    pwr(t, Seq(Seq.empty[A])) //Powerset of ∅ is {∅}
  }

  def suggestKeeps(hand: Hand): SuggestedKeep = {
    val possibleKeeps = power(hand.cards).map(_.toSeq)
    possibleKeeps
      .filter(heuristics)
      .map(k => new SuggestedKeep(k, expectedValue(hand, k)))
      .maxBy(_.expectedValue)
  }

  private def heuristics(keeps: Seq[Card]) : Boolean = {
    if (keeps.size == 1 && keeps.head.rank > 1 && keeps.head.rank < 11) {
      return false
    }
    if (keeps.size == 2
      && keeps(0).suit != keeps(1).suit
      && keeps(0).rank != keeps(1).rank
      && keeps.exists(k => k.rank > 1 && k.rank < 10)) {
      return false
    }
    return true
  }

  val memoizedFillsBySize = new mutable.HashMap[Int, Seq[Seq[Card]]]

  private def expectedValueBruteForce(hand: Hand, keeps: Seq[Card]): Double = {
    if (!memoizedFillsBySize.contains(keeps.size)) {
      memoizedFillsBySize(keeps.size) = new HandFiller().PartialFills(hand, 5 - keeps.size)
    }
    val fills = memoizedFillsBySize(keeps.size).map(fill => fill ++ keeps)
    fills.par.map(f => PayTable.getValue(new Hand(f))).sum.toFloat / fills.size
  }

  def evalTwo(cards: Seq[Card]): Double = {
    val totalThrees = 19600.0
    if (cards(0).rank == cards(1).rank) {
      val threesOfAKind = 12*4
      val pickUpThird = 6
      val pickUpFour = 3
      val highPair = 1
    }
    0
  }

  def evalThree(cards: Seq[Card]): Double = {
    0
  }

  private def expectedValue(hand: Hand, keeps: Seq[Card]): Double = {
    keeps.size match {
      case 0 => 6.387
      case 1 => 7.854 // assuming it's a high card keep
      case 2 => expectedValueBruteForce(hand, keeps)
      case 3 => expectedValueBruteForce(hand, keeps)
      case 4 => expectedValueBruteForce(hand, keeps)
      case 5 => PayTable.getValue(new Hand(keeps))
    }
  }

}

case class SuggestedKeep(val keeps: Seq[Card], val expectedValue: Double)