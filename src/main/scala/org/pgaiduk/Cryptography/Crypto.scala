package org.pgaiduk.Cryptography

import scala.collection.mutable
import scala.util.Random

object Crypto {
  val rand = new Random(System.currentTimeMillis())
  val greatest_val = 1000000000

  def get_test_number() : Long = {
    var n = math.abs(rand.nextLong()) % greatest_val
    while (n == 0){
      n = math.abs(rand.nextLong()) % greatest_val
    }
    n
  }
  def get_test_prime_number() : Long = {
    var q = math.abs(rand.nextLong()) % greatest_val
    while (!fermi(q)) {
      q = math.abs(rand.nextLong()) % greatest_val
    }
    q
  }

  def FME(x:BigInt, y:BigInt, N:BigInt) : BigInt = {
    if (y == 0)
      1
    else
      if (y % 2 == 0)
        FME(x, y / 2, N).pow(2) % N
      else {
        (x * FME(x, y / 2, N).pow(2)) % N
      }
  }

  def gcd(a:BigInt, b:BigInt): (BigInt, BigInt, BigInt) = {
    if (a == 0) {
      return (b, 0, 1)
    }
    val m = gcd(b%a, a)
    (m._1, m._3 - (b / a) * m._2, m._2)
  }

   private def fermi(a:BigInt) : Boolean = {
     if (a == 2)
        return true
    val r = new Random(System.currentTimeMillis())
    for (i <- 1 to 100){
      val t:BigInt = math.abs((r.nextLong() % (a - 2) + 2).toLong)
      if (FME(t, a - 1, a) != 1)
        return false
    }
    true
  }

  private def generateSophieGermain(): (BigInt, BigInt) = {
    var exit = false
    var q:BigInt = 0
    while (!exit) {
      q = math.abs(rand.nextLong())
      while (!fermi(q)) {
        q = math.abs(rand.nextLong()) % greatest_val
      }
      if (fermi(2 * q - 1)) {
        exit = true
      }
    }
    (2 * q - 1, q)
  }

  def DHf() : DHValues = {
    val pq = generateSophieGermain()
    var g = math.abs(rand.nextLong()) % pq._1
    var flag = true
    while (g < 2 && flag){
      g = math.abs(rand.nextLong()) % pq._1
      flag = false
      if (FME(g, pq._2, pq._1) == 1)
        flag = true
    }
    val Xs = (math.abs(rand.nextLong()) % greatest_val, math.abs(rand.nextLong()) % greatest_val)
    val Ys = (FME(g, Xs._1, pq._1), FME(g, Xs._2, pq._1))
    val Z_ab = FME(Ys._2, Xs._1, pq._1)
    val Z_ba = FME(Ys._1, Xs._2, pq._1)
    println("Z_ab = " + Z_ab + " Z_ba = " + Z_ba)
    new DHValues(Xs._1, Xs._2, Ys._1, Ys._2, Z_ab)
  }

//  def GS(a:BigInt, b:BigInt, p:BigInt): BigInt = {
//    if (b > p)
//      return -1
//    var m:BigInt = (scala.math.sqrt(p.toDouble) + 1).toLong
//    val map:HashMap[BigInt, BigInt] = {
//      new mutable.HashMap[BigInt, BigInt]()
//    }
//    for (i <- m.toInt to 0 by -1) {
//      map += (FME(a, i * m, p) -> BigInt(i))
//    }
//    for (i <- 0 to m.toInt) {
//      val current:BigInt = FME(a, i, p) * b % p
//      if (map.exists(_._1 == current))
//        if (map(current) * m - i < p)
//          return map(current) * m - i
//    }
//    -1
//  }

  def GSs(a:BigInt, b:BigInt, p:BigInt): String = {
    if (b > p)
        return "log" + a + s"($b) mod $p = Undefined"
    val m:Int = (scala.math.sqrt(p.toDouble) + 1).toInt
    val map:mutable.HashMap[BigInt, BigInt] = new mutable.HashMap[BigInt, BigInt]()
    for (i <- 1 to m) {
      map += (FME(a, i * m, p) -> i)
    }
    for (j <- 0 until m) {
      val current:BigInt = FME(a, j, p) * b % p
      if (map.exists(_._1 == current))
        if ((map(current) * m - j)%p < p)
          return "log" + a + s"($b) mod $p = ${map(current) * m - j}"
    }
    "log" + a + s"($b) mod $p = Undefined"
  }
}
