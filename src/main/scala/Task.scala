import org.pgaiduk.Cryptography.Crypto
import org.pgaiduk.Cryptography.Crypto_ciphers

object Task {

  def main (args: Array[String]) : Unit = {
//    Crypto_ciphers.Shamir_cipher("resourses/text_to_encrypt")
//    Crypto_ciphers.El_Gamal_cipher("resourses/text_to_encrypt")
//    Crypto_ciphers.Vernam_cipher("resourses/text_to_encrypt")

    val keys = Crypto_ciphers.RSA_generate_keys()
    val message = Crypto_ciphers.RSA_encrypt("resourses/text_to_encrypt", keys._1, keys._2)
    println(s"encrypted message = $message")
    print(s"c = ${keys._3}\nd = ${keys._1}\nN = ${keys._2}\n")
    println(s"decrypted message = ${Crypto_ciphers.RSA_decrypt(message, keys._3, keys._2)}")
  }

}