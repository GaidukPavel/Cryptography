package org.pgaiduk.poker

import org.pgaiduk.Cryptography.Crypto

import scala.util.Random

class Player(pc: BigInt) {
  var c:BigInt = 0
  val p:BigInt = pc
  while (Crypto.gcd(c, p - 1)._1 != 1) {
    c = Crypto.gen_number(128)
  }
  val d =
    {
      if (Crypto.gcd(c, p - 1)._2 > 0)
        Crypto.gcd(c, p - 1)._2
      else
        Crypto.gcd(c, p - 1)._2 + p - 1
    }
  var deck = Array[BigInt]()
  var hand = Array[BigInt]()
  def enc(): Unit = {
    for (i <- 0 to deck.length) {
      deck(i) = Crypto.FME(deck(i), c, p)
    }
  }

  def enc2(): Unit = {
    for (i <- 0 to deck.length) {
      deck(i) = Crypto.FME(deck(i), d, p)
    }
  }

  def dec(): Unit = {
    for (i <- 0 to 2){
      hand(i) = Crypto.FME(hand(i), d, p)
    }
  }

  def shuffle(): Unit = {
    deck = Random.shuffle(deck.toList).toArray
  }

}
