package org.pgaiduk.vote

import org.pgaiduk.Cryptography.{Crypto, Crypto_ciphers, Crypto_signatures}

class Server(participants:Int) {
  private val partis: Int = {
    if (participants == 1)
      2
    else
      participants
  }

  private val a = Crypto_ciphers.RSA_generate_keys()

  val candidates:List[String] = List[String]("Java", "C++", "Scala", "Python", "Swift")
  var box:List[(BigInt, BigInt)] = List[(BigInt, BigInt)]()
  var voters:Array[BigInt] = new Array[BigInt](partis)
  for (i <- 0 until partis){
    voters(i) = i
  }

  def get_number_of_candidates():Int = {
    candidates.length
  }

  def get_server_key(): (BigInt, BigInt) = {
    (a._1, a._2)
  }

  def vote(enc:BigInt, signed:BigInt, key:BigInt, N:BigInt): Unit = {
    if (voters.contains(key) && Crypto_signatures.RSA_verify(signed, key, N, Crypto_ciphers.RSA_decrypt(enc, a._3, a._2).toInt))
      box = (enc, signed) :: box
  }

  def check_voter(number:Int, key:BigInt): Boolean ={
    if (voters.contains(number)){
      for (i <- voters.indices){
        voters(i) = key
        return true
      }
    }
    false
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
