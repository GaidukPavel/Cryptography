package org.pgaiduk.poker

import org.pgaiduk.Cryptography.Crypto

import scala.collection.mutable

class Game(pl:Int) {
  val players: Int = {
    if (pl > 23)
      23
    else if (pl < 2)
      2
    else
      pl
  }
  val p:BigInt = Crypto.gen_prime(128)
  var pArr: Array[Player] = Array[Player]()
  for (i <- 0 until players){
    pArr = pArr :+ new Player(p, i)
  }
  println(s"size = ${pArr.length}")
  val suit:Array[Char] = Array[Char]('♠','♡','♣','♢')
  val card_names:Array[String] = Array[String]("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
  var cards: mutable.Map[BigInt, Int] = mutable.Map[BigInt, Int]()
  for (i <- 0 to 51){
    cards += ((Crypto.gen_test_number(), i))
  }

  var deck: List[BigInt] = List[BigInt]()
  for (i <- cards){
    deck = i._1 :: deck
  }
  for (player <- pArr) {
    player.deck = deck
    player.enc()
    deck = player.deck
  }

  var table:List[BigInt] = deck.take(5)
  var encr:List[BigInt] = table
  for (player <- pArr) {
    player.deck = table
    player.dec()
    table = player.deck
  }
  for (i <- 0 until 5)
    deck = deck.tail
  print("Table: ")
  for (i <- table){
    print(s"${card_names(cards(i) % 13)}${suit(cards(i) % 4)} ")
  }
  println

  for (i <- 0 until players){
    pArr(i).hand = deck.take(2)
    for (i <- 0 until 2)
    deck = deck.tail
  }

  for (player <- pArr){
    var tmp:List[BigInt] = player.hand
    for (eachp <- pArr) {
      eachp.deck = tmp
      eachp.dec()
      tmp = eachp.deck
    }
    player.hand = tmp
    println(s"Player${player.number}: " +
      s" ${card_names(cards(player.hand.head) % 13)}${suit(cards(player.hand.head) % 4)}" +
      s" ${card_names(cards(player.hand.tail.head) % 13)}${suit(cards(player.hand.tail.head) % 4)}")
  }

  println(s"deck length = ${deck.length}")


}
