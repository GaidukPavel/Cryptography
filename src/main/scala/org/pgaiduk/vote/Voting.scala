package org.pgaiduk.vote

import org.pgaiduk.Cryptography.{Crypto, Crypto_ciphers, Crypto_signatures}

class Voting(participants:Int) {
  private val partis: Int = {
    if (participants == 1)
      2
    else
      participants
  }

  private val a = Crypto_ciphers.RSA_generate_keys()
  private var e = List[(BigInt /*d*/, BigInt /*N*/, BigInt /*c*/)]()
  for (i <- 0 until partis)
    e = Crypto_ciphers.RSA_generate_keys() :: e

  val candidates:List[String] = List[String]("Java", "C++", "Scala", "Python", "Swift")
  var box:List[(BigInt, BigInt)] = List[(BigInt, BigInt)]()

  def make_vote(): Unit = {
    for (participant <- e) {
      val vote:Long = Crypto.gen_test_number() % candidates.length
      val keys = Crypto_ciphers.RSA_generate_keys()
      box = (Crypto_ciphers.RSA_encrypt(vote, a._1, a._2), Crypto_signatures.RSA_sign(vote, participant._2, participant._3)) :: box
    }
  }

  def count_results(): Unit = {
    val results: Array[Int] = Array.fill(candidates.length){0}
    var i:Int = 0
    for (bulletin <- box) {
      results(Crypto_ciphers.RSA_decrypt(bulletin._1, a._3, a._2).toInt) += 1
    }
    for (i <- results zip candidates)
      println(i._1 + " votes for " + i._2)
  }
}
