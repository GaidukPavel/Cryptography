
import org.pgaiduk.Cryptography.Crypto
import org.pgaiduk.Cryptography.Crypto_ciphers

object Task {

  def main (args: Array[String]) : Unit = {
    Crypto_ciphers.Vernam_cipher("resources/picture.png")
//    val keys = Crypto_ciphers.RSA_generate_keys()
//    val message = Crypto_ciphers.RSA_encrypt("resources/picture.png", keys._1, keys._2)
//    Crypto_ciphers.RSA_decrypt(message, keys._3, keys._2, "resources/decrypt.png")
  }

}