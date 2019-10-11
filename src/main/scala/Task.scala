import org.pgaiduk.Cryptography.Crypto

object Task {

  def main (args: Array[String]) : Unit = {
//    Crypto.Shamir_cipher("Hello, B abonent. How are you?")
//    Crypto.El_Gamaal_cipher("Hello, B abonent.")
      Crypto.RSA("Hello, B!")
  }
}