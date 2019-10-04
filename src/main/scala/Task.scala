import org.pgaiduk.Cryptography.Crypto

object Task {

  def main (args: Array[String]) : Unit = {
    for (i <- 0 to 10) {
      val n = Crypto.get_test_number()
      val m = Crypto.get_test_number()
      val p = Crypto.get_test_prime_number()
      println(s"n = $n, m = $m, p = $p")
      println(s"$n^$m % $p = ${Crypto.FME(n, m, p)}")
    }

    for (i <- 0 to 10) {
      val a = Crypto.get_test_prime_number()
      val b = Crypto.get_test_prime_number()
      println(s"a = $a, b = $b")
      println(s"gcd = ${Crypto.gcd(a, b)}")
    }

  }
}