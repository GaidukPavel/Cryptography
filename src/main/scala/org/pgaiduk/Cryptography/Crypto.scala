package org.pgaiduk.Cryptography

import scala.collection.mutable
import scala.util.Random

object Crypto {
  val rand = new Random(System.currentTimeMillis())
  val greatest_val = 1000000000

  def fermi(a:BigInt) : Boolean = {
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

  def generateSophieGermain(): (BigInt, BigInt) = {
    var exit = false
    var q:BigInt = 0
    while (!exit) {
      q = math.abs(rand.nextLong())
      while (!fermi(q)) q = math.abs(rand.nextLong()) % greatest_val
      if (fermi(2 * q + 1)) exit = true
    }
    (2 * q + 1, q)
  }

  def generateSophieGermain(gt: BigInt): (BigInt, BigInt) = {
    var exit = false
    var q:BigInt = 0
    while (!exit) {
      q = gen_test_prime_number(gt)
      if (fermi(2 * q + 1)) exit = true
    }
    (2 * q + 1, q)
  }

  def gen_test_number() : Long = {
    var n = math.abs(rand.nextLong()) % greatest_val
    while (n == 0){
      n = math.abs(rand.nextLong()) % greatest_val
    }
    n
  }

  def gen_prime(bits:Int) : BigInt = {
    var num:BigInt = 10
    while (!fermi(num) || num.bitLength != bits){
      num = 0
      for (i <- 1 until bits) {
        num += math.abs(rand.nextLong()) % 2
        num <<= 1
      }
      num += 1
    }
    num
  }

  def gen_number(bits:Int) : BigInt = {
    var num:BigInt = 1
    for (i <- 1 to bits) {
      num += math.abs(rand.nextLong()) % 2
      num <<= 1
    }
    num >>= 1
    num
  }

  def gen_test_number(highest:BigInt) : BigInt = {
    var n:BigInt = (BigInt(math.abs(rand.nextLong())) * BigInt(math.abs(rand.nextLong())) * BigInt(math.abs(rand.nextLong()))) % highest
    while (n == 0 || n == 1) {
      n = (BigInt(math.abs(rand.nextLong())) * BigInt(math.abs(rand.nextLong())) * BigInt(math.abs(rand.nextLong()))) % highest
    }
    n
  }

  def gen_test_prime_number() : Long = {
    var q = math.abs(rand.nextLong()) % greatest_val
    while (!fermi(q)) {
      q = math.abs(rand.nextLong()) % greatest_val
    }
    q
  }

  def gen_test_prime_number(gt:BigInt) : BigInt = {
    var q:BigInt = math.abs(rand.nextLong())
    while (q < gt){
      q *= math.abs(rand.nextLong())
      q += math.abs(rand.nextLong())
    }
    while (!fermi(q)) {
      q = math.abs(rand.nextLong())
      while (q < gt) {
        q *= math.abs(rand.nextLong())
        q += math.abs(rand.nextLong())
      }
    }
    q
  }

  def gen_inversion(a:BigInt, p:BigInt): BigInt ={
    FME(a, p - 2, p)
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

  def gcd(a:BigInt, b:BigInt): (BigInt /*g*/, BigInt /*x*/, BigInt /*y*/) = {
    if (a == 0) {
      return (b, 0, 1)
    }
    val m = gcd(b%a, a)
    (m._1, m._3 - (b / a) * m._2, m._2)
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

