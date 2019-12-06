import org.pgaiduk.Cryptography.Crypto_ciphers
import org.pgaiduk.Cryptography.Crypto_signatures
import org.pgaiduk.poker.Game
import org.pgaiduk.vote.Voting
import org.pgaiduk.zeroKnowledge

object Task {

  def main (args: Array[String]) : Unit = {
//    Crypto_ciphers.Vernam_cipher("resources/picture.png")
//    val keys = Crypto_ciphers.RSA_generate_keys()
//    val message = Crypto_ciphers.RSA_encrypt("resources/picture.png", keys._1, keys._2)
//    Crypto_ciphers.RSA_decrypt(message, keys._3, keys._2, "resources/decrypt.png")
//    val sign = Crypto_signatures.RSA_sign("resources/picture.png")
//    Crypto_signatures.RSA_verify("resources/picture.png", sign._1, sign._2, sign._3)
//    val sign = Crypto_signatures.El_Gamal_sign("resources/picture.png")
//    Crypto_signatures.El_Gamal_verify("resources/picture.png", sign._1, sign._2, sign._3, sign._4, sign._5)
//      var sign = Crypto_signatures.Interstate_Standard_sign("resources/picture.png")
//      Crypto_signatures.Interstate_Standard_verify("resources/text_to_encrypt", sign._1, sign._2, sign._3, sign._4, sign._5)
    /** Poker **/
//    println(s"Input number of players: ")
//    val players:String = scala.io.StdIn.readLine()
//    val game:Game = new Game(players.toInt)
    /***********/
    /****Voting****/
//    val voting:Voting = new Voting(600)
//    voting.make_vote()
//    voting.count_results()
    /********/
    /***Zero-knowledge***/
    val knowledge = new zeroKnowledge.GraphColoring
    for (i <- 0 until 13){
      println(s"result = ${knowledge.checkLink(i)}")
    }
    knowledge.print_graph()
    /********/
  }

}