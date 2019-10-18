package org.pgaiduk.Cryptography

import java.nio.file.{Files, Paths}

object Crypto_ciphers {

  def Shamir_cipher(path:String): Unit ={
    val byteArray = Files.readAllBytes(Paths.get(path))
    var p = Crypto.gen_test_prime_number()
    var decoded_message:String = ""
    for (sym <- byteArray) {
      val Ca = Crypto.gen_test_prime_number()
      var inversion = Crypto.gcd(Ca, p - 1)
      val Da = if (inversion._2 < 1)
        inversion._2 + p - 1
      else
        inversion._2
      //      println(s"Debug: Ca = $Ca, Da = $Da, p - 1 = ${p - 1}")

      val Cb = Crypto.gen_test_prime_number()
      inversion = Crypto.gcd(Cb, p - 1)
      val Db = if (inversion._2 < 1)
        inversion._2 + p - 1
      else
        inversion._2
      //      println(s"Debug: Cb = $Cb, Db = $Db, p - 1 = ${p - 1}")

      val x1 = Crypto.FME(sym.toInt, Ca, p) // transfer from A to B

      val x2 = Crypto.FME(x1, Cb, p) // transfer from B to A

      val x3 = Crypto.FME(x2, Da, p) // transfer from A to B

      val x4 = Crypto.FME(x3, Db, p)

      decoded_message += x4.toChar
//      println(s"B abonent get ${x4.toChar} symbol")
    }
    println("Decoded message = " + decoded_message)
  }

  def El_Gamal_cipher(path:String): Unit = {
    val byteArray = Files.readAllBytes(Paths.get(path))
    var pg = Crypto.generateSophieGermain()

    var c1:BigInt = 0
    var c2:BigInt = 0
    c1 = Crypto.gen_test_number(pg._1 - 1)
    c2 = Crypto.gen_test_number(pg._1 - 1)

    val d1:BigInt = Crypto.FME(pg._2, c1, pg._1)
    val d2:BigInt = Crypto.FME(pg._2, c2, pg._1)

    for (sym <- byteArray){
      val k = Crypto.gen_test_number(pg._1 - 2)
      val r = Crypto.FME(pg._2, k, pg._1) // counted by A
      /*
      d1, d2 - public keys
      c1, c2 - private keys
      */
      val e = (sym.toInt % pg._1 * Crypto.FME(d2, k, pg._1)) % pg._1 // counted by A

      val o = (e % pg._1 * Crypto.FME(r, pg._1 - 1 - c2, pg._1)) % pg._1 // counted by B
//      println(s"Abonent B get ${o.toChar} symbol")
      print(s"${o.toChar}")
    }
    println()
  }

  def RSA(path:String): Unit ={
    val byteArray = Files.readAllBytes(Paths.get(path))
    val P1:BigInt = Crypto.gen_test_prime_number()
    val Q1:BigInt = Crypto.gen_test_prime_number()
    val N1:BigInt = P1 * Q1

    val fi1 = (P1 - 1) * (Q1 - 1)
    var f:Boolean = true
    var d1:BigInt = 0
    while (f) {
      d1 = Crypto.gen_test_prime_number()
      if (Crypto.gcd(d1, fi1)._1 == 1)
        f = false
    }
    var c1 = Crypto.gcd(d1, fi1)._2
    if (c1 < 0) c1 += fi1
    //    println(s"Debug: c1 = $c1, d1 = $d1, fi1 = $fi1")

    val P2:BigInt = Crypto.gen_test_prime_number()
    val Q2:BigInt = Crypto.gen_test_prime_number()
    val N2:BigInt = P2 * Q2

    val fi2 = (P2 - 1) * (Q2 - 1)
    f = true
    var d2:BigInt = 0

    while (f) {
      d2 = Crypto.gen_test_prime_number()
      if (Crypto.gcd(d2, fi2)._1 == 1)
        f = false
    }
    var c2 = Crypto.gcd(d2, fi2)._2
    if (c2 < 0) c2 += fi2
    //    println(s"Debug: c2 = $c2, d2 = $d2, fi2 = $fi2")
    var decoded_message:String = ""
    var encrypted_message:BigInt = 0
    var offset:Int = 0
    var N2_cp = N2
    var offset_mask:BigInt = 0
    while (N2_cp != 0){
      N2_cp >>= 1
      offset += 1
    }
    for (i <- 0 until offset){
      offset_mask <<= 1
      offset_mask |= 1
    }
    for (sym <- byteArray){
      encrypted_message += Crypto.FME(sym.toInt, d2, N2)
      var p = Crypto.FME(sym.toInt, d2, N2)
      encrypted_message <<= offset
    }

    for (i <- 0 to byteArray.size){
      var c:Char = Crypto.FME(encrypted_message &  offset_mask, c2, N2).toChar
      encrypted_message >>= offset
      decoded_message += c
    }
    println(s"Decoded message = ${decoded_message.reverse}")
  }

  def Vernam_cipher(path:String): Unit ={
    val byteArray = Files.readAllBytes(Paths.get(path))
    var len:Int = byteArray.size
    var message:BigInt = 0
    for (sym <- byteArray) {
      message += sym.toInt
      message <<= 8
    }
    var key:BigInt = 0
    for (i <- 0 to len) {
      key += Crypto.gen_test_number() % 255
      key <<= 8
    }

    var encrypted:BigInt = message ^ key
    var decrypted = encrypted ^ key
    var dec_mes:String = ""
    for (i <- 0 to len){
      dec_mes += (decrypted & 0xff).toChar
      decrypted >>= 8
    }
    println("Decrypted message = " + dec_mes.reverse)
  }
}
