package org.pgaiduk.poker

import org.pgaiduk.Cryptography.Crypto
import org.pgaiduk.Cryptography.Crypto.rand

class Game(pl:Int) {
  val players: Int = {
    if (pl > 23)
      23
    else
      pl
  }
  val p:BigInt = Crypto.gen_prime(128)
  val pArr: Array[Player] = Array[Player]()
  for (i <- 0 to players){
    pArr :+ new Player(p)
  }
  val cards: Array[BigInt] = Array[BigInt]()
  for (i <- 0 to 51){
    cards :+ Crypto.gen_number(30)
  }
  pArr(0).deck = cards

  pArr(0).enc()
  pArr(0).shuffle()
  pArr((1) % pArr.length).deck = pArr(0).deck

  var cn:Int = math.abs(rand.nextInt()) % pArr((1) % pArr.length).deck.length
  pArr(0).hand :+ pArr((1) % pArr.length).deck(cn)
  pArr(0).hand :+ pArr((1) % pArr.length).deck.drop(cn)

  cn = math.abs(rand.nextInt()) % pArr((1) % pArr.length).deck.length
  pArr(0).hand :+ pArr((1) % pArr.length).deck(cn)
  pArr(0).hand :+ pArr((1) % pArr.length).deck.drop(cn)

  for (i <- 0 to pArr.length){



  }
  println
  for (player <- pArr) {

  }
}
