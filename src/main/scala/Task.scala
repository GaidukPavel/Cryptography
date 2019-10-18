import org.pgaiduk.Cryptography.Crypto
import org.pgaiduk.Cryptography.Crypto_ciphers

object Task {

  def main (args: Array[String]) : Unit = {
    Crypto_ciphers.Shamir_cipher("resourses/text_to_encrypt")
    Crypto_ciphers.El_Gamal_cipher("resourses/text_to_encrypt")
    Crypto_ciphers.RSA("resourses/text_to_encrypt")
    Crypto_ciphers.Vernam_cipher("resourses/text_to_encrypt")
  }

}