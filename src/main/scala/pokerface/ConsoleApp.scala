package pokerface

import scala.io.StdIn

/**
 * Created by Eliah on 9/7/2014.
 */
 object ConsoleApp extends App {

  val suggester = new Suggester

  while (true) {
    print("Input hand: ")
    val handStr = StdIn.readLine()
    val hand = Hand.parse(handStr)
    if (hand.isEmpty) {
      println("Couldn't parse that hand. Please try again.")
    }
    else {
      val sugg = suggester.suggestKeeps(Hand.parse(handStr).get)
      val keeps = sugg.keeps
      val ev = sugg.expectedValue
      println(s"Keep $keeps with an EV of $ev")
    }
  }

}
