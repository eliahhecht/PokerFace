package pokerface

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

  def suggestKeeps(hand: Hand): Seq[Card] = {
    val possibleKeeps = power(hand.cards).map(_.toSeq).filter(_.size > 1)
    possibleKeeps.maxBy(expectedValue(hand, _))
  }

  private def expectedValue(hand: Hand, keeps: Seq[Card]): Double = {
    val fills = new HandFiller().AllFills(hand, keeps.toSeq)
    fills.map(PayTable.getValue).sum.toFloat / fills.size
  }

}
