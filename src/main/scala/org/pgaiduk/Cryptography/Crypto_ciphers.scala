package org.pgaiduk.Cryptography

import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.file.{Files, Paths}

object Crypto_ciphers {

  private def write_to_file(message:String, path:String): Unit = {
    val bos = new BufferedOutputStream(new FileOutputStream(path))
    for (byte <- message) {
      Stream.continually(bos.write(byte))
    }
    bos.close()
  }

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

      val Cb = Crypto.gen_test_prime_number()
      inversion = Crypto.gcd(Cb, p - 1)
      val Db = if (inversion._2 < 1)
        inversion._2 + p - 1
      else
        inversion._2

      val x1 = Crypto.FME(sym.toInt, Ca, p) // transfer from A to B

      val x2 = Crypto.FME(x1, Cb, p) // transfer from B to A

      val x3 = Crypto.FME(x2, Da, p) // transfer from A to B

      val x4 = Crypto.FME(x3, Db, p)

      decoded_message += x4.toChar
    }
    write_to_file(decoded_message, "resources/shamir_decrypt")
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

    var decrypted_message:String = ""

    for (sym <- byteArray){
      val k = Crypto.gen_test_number(pg._1 - 2)
      val r = Crypto.FME(pg._2, k, pg._1) // counted by A
      /*
      d1, d2 - public keys
      c1, c2 - private keys
      */
      val e = (sym.toInt % pg._1 * Crypto.FME(d2, k, pg._1)) % pg._1 // counted by A

      val o = (e % pg._1 * Crypto.FME(r, pg._1 - 1 - c2, pg._1)) % pg._1 // counted by B
      decrypted_message += o.toChar
    }
    write_to_file(decrypted_message, "resources/El_Gamal_decrypt")
  }

  def RSA_generate_keys(): (BigInt /*d*/, BigInt /*N*/, BigInt /*c*/) = {
    val P:BigInt = Crypto.gen_test_prime_number()
    val Q:BigInt = Crypto.gen_test_prime_number()
    val N:BigInt = P * Q
    val fi = (P - 1) * (Q - 1)

    var f:Boolean = true
    var d:BigInt = 0
    while (f) {
      d = Crypto.gen_test_prime_number()
      if (Crypto.gcd(d, fi)._1 == 1)
        f = false
    }
    var c:BigInt = Crypto.gcd(d, fi)._2
    if (c < 0) c += fi
    (d, N, c)
  }

  def RSA_count_offset(N:BigInt): (Int, BigInt) = {
    var offset:Int = 0
    var N_cp = N
    var offset_mask:BigInt = 0
    while (N_cp != 0){
      N_cp >>= 1
      offset += 1
    }
    for (i <- 0 until offset){
      offset_mask <<= 1
      offset_mask |= 1
    }
    (offset, offset_mask)
  }

  def RSA_encrypt(path:String, d:BigInt, N:BigInt): BigInt = {
    val byteArray = Files.readAllBytes(Paths.get(path))
    var encrypted_message:BigInt = 0
    for (sym <- byteArray){
      val sym_int:Int = sym + 128
      encrypted_message += Crypto.FME(sym_int.toInt, d, N)
      encrypted_message <<= RSA_count_offset(N)._1
    }
    encrypted_message >>= RSA_count_offset(N)._1
    encrypted_message
  }

  def RSA_decrypt(message:BigInt, c:BigInt, N:BigInt, path:String): Unit ={
    var decoded_message:String = ""
    var message_cp:BigInt = message
    while (message_cp != 0){
      var byte:Char = (Crypto.FME(message_cp & RSA_count_offset(N)._2, c, N) - 128).toChar
      message_cp >>= RSA_count_offset(N)._1
      decoded_message += byte
    }
    decoded_message.reverse
    decoded_message.dropRight(1)
    write_to_file(decoded_message, "resources/RSA_decrypt")
  }

  def Vernam_cipher(path:String): Unit ={
    val byteArray = Files.readAllBytes(Paths.get(path))
    val len:Int = byteArray.size
    val key = new Array[Byte](len)
    for (i <- 0 until len)
      key(i) = (Crypto.gen_test_number() % 255).toByte
    val encrypted = new Array[Byte](len)
    for (i <- 0 until len)
      encrypted(i) = byteArray(i).^(key(i)).toByte
    val decrypted = new Array[Byte](len)
    for (i <- 0 until len)
      decrypted(i) = encrypted(i).^(key(i)).toByte
    write_to_file(decrypted.map(_.toChar).mkString, "resources/Vernam_decrypt")
  }

}
