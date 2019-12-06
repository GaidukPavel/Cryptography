package org.pgaiduk.vote

import org.pgaiduk.Cryptography.{Crypto, Crypto_ciphers, Crypto_signatures}

class Client(data:Int, server:Server) {
  val e: (BigInt, BigInt, BigInt) = Crypto_ciphers.RSA_generate_keys()
  val n: Int = data
  def vote(): Unit ={
    if (server.check_voter(n, e._1)){
      val vote:Long = Crypto.gen_test_number() % server.get_number_of_candidates()
      server.vote(Crypto_ciphers.RSA_encrypt(vote, server.get_server_key()._1, server.get_server_key()._2), Crypto_signatures.RSA_sign(vote, e._2, e._3), e._1, e._2)
    }
  }



}
