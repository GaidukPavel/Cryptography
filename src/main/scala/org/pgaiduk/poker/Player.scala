package org.pgaiduk.poker

import org.pgaiduk.Cryptography.Crypto

import scala.util.Random

class Player(pc: BigInt, num: Int) {
  var c:BigInt = 0
  val p:BigInt = pc
  val number:Int = num
  while (Crypto.gcd(c, p - 1)._1 != 1) {
    c = Crypto.gen_number(128)
  }
  val d: BigInt =
    {
      if (Crypto.gcd(c, p - 1)._2 > 0)
        Crypto.gcd(c, p - 1)._2
      else
        Crypto.gcd(c, p - 1)._2 + p - 1
    }
  var deck: List[BigInt] = List[BigInt]()
  var hand: List[BigInt] = List[BigInt]()
  def enc(): Unit = {
    deck = deck.map((i: BigInt) => Crypto.FME(i, d, p))
    shuffle()
  }

  def dec(): Unit = {
    deck = deck.map((i: BigInt) => Crypto.FME(i, c, p))
  }

  def shuffle(): Unit = {
    deck = Random.shuffle(deck)
  }

  def getC:BigInt = {
    d
  }

}
