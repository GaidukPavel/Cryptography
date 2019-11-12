package org.pgaiduk.Cryptography

import java.nio.file.{Files, Paths}

import org.pgaiduk.Cryptography.Crypto.rand

/*
    Hex string to BigInt
    println("digest str = " + digestStr)
    val bs = new BigInteger(digestStr, 16)
* */

object Crypto_signatures {

  private def read_file(path:String): String = {
    val byteArray = Files.readAllBytes(Paths.get(path))
    var message:String = ""
    for (sym <- byteArray) {
      message += sym.toChar
    }
    message
  }

  private def generate_pq(): Unit = {
//    val q = Crypto.gen_prime(256)
//    var b:BigInt = 1
//    while ((q*b + 1).bitLength != 1024 && !Crypto.fermi(q*b + 1)){
//      b = Crypto.gen_prime(767)
//      if ((b*q + 1).bitLength < 1024)
//        b = b * 2 * (1024 - (b*q + 1).bitLength)
//    }
//    TODO pq 16 31

  }

  def El_Gamal_sign(path:String): (BigInt, BigInt, BigInt, BigInt, BigInt) = {
    val h = new SHA1(read_file(path))
    val pg = Crypto.generateSophieGermain(BigInt(2).<<(160))
    val x:BigInt = Crypto.gen_test_number(pg._1 - 1) // private
    val y:BigInt = Crypto.FME(pg._2, x, pg._1) // public
    var k: BigInt = 0
    while (Crypto.gcd(k, pg._1 - 1)._1 != 1)
      k = Crypto.gen_test_number(pg._1 - 1)
    val r:BigInt = Crypto.FME(pg._2, k, pg._1)
    println(s"x = $x")
    println(s"y = $y")
    println(s"p = ${pg._1}")
    var u:BigInt = (h.getSHA1 - x*r) % (pg._1 - 1)
    if (u < 1)
      u = u + pg._1 - 1
    val inversion:BigInt =
      {
        if (Crypto.gcd(k, pg._1 - 1)._2 > 1)
          Crypto.gcd(k, pg._1 - 1)._2
        else
          Crypto.gcd(k, pg._1 - 1)._2 + pg._1 - 1
      }
    val s:BigInt = (inversion * u) % (pg._1 - 1)
    (r, s, y, pg._1, pg._2)
  }

  def El_Gamal_verify(path:String, r:BigInt, s:BigInt, y:BigInt, p:BigInt, g:BigInt): Unit ={
    val h = new SHA1(read_file(path))
    println("h = " + h.getSHA1)
    val et:BigInt = Crypto.FME(g, h.getSHA1, p)
    val te:BigInt = Crypto.FME(y, r, p) * Crypto.FME(r, s, p) % p
    //*****Debug
    if (r < 0 || r >= p)
      println(s"wrong: p = $p; r = $r")
    if (s < 0 || s >= p-1)
      println(s"wrong: s = $p; p - 1 = ${p - 1}")
    //**********
    println(s"(y^r * r^s) % p = $et")
    println(s"(  g^h  )   % p = $te")
  }

  def RSA_sign(path:String): (BigInt, BigInt /*d*/, BigInt /*N*/) = {
    val h = new SHA1(read_file(path))
    val params = Crypto_ciphers.RSA_generate_keys()
    val s:BigInt = Crypto.FME(h.getSHA1 % params._2, params._3, params._2)
    (s, params._1, params._2)
  }

  def RSA_verify(path:String, s:BigInt,d:BigInt, N:BigInt): Unit = {
    val h = new SHA1(read_file(path))
    val w:BigInt = Crypto.FME(s, d, N)
    println(s"h = ${h.getSHA1 % N}\nw = $w")
  }

  def Interstate_Standard_sign(path:String) = {

  }

  def Interstate_Standard_verify(path:String, y:BigInt, a:BigInt, r:BigInt, s:BigInt, p:BigInt, q:BigInt): Unit = {

  }

}
