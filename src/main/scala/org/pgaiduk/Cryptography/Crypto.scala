package org.pgaiduk.Cryptography

import scala.collection.mutable
import scala.util.Random

object Crypto {
  val rand = new Random(System.currentTimeMillis())
  val greatest_val = 1000000000

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
      while (!fermi(q)) q = math.abs(rand.nextLong()) % greatest_val
      if (fermi(2 * q - 1)) exit = true
    }
    (2 * q - 1, q)
  }

  def gen_test_number() : Long = {
    var n = math.abs(rand.nextLong()) % greatest_val
    while (n == 0){
      n = math.abs(rand.nextLong()) % greatest_val
    }
    n
  }

  def gen_test_number(highest:BigInt) : BigInt = {
    var n = math.abs(rand.nextLong()) % highest
    while (n == 0 || n == 1) n = math.abs(rand.nextLong()) % highest
    n
  }

  def gen_test_prime_number() : Long = {
    var q = math.abs(rand.nextLong()) % greatest_val
    while (!fermi(q)) {
      q = math.abs(rand.nextLong()) % greatest_val
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

  def gcd(a:BigInt, b:BigInt): (BigInt, BigInt, BigInt) = {
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

  def Shamir_cipher(m:String): Unit ={
    var p = gen_test_prime_number()

    for (sym <- m) {
      val Ca = gen_test_prime_number()
      var inversion = gcd(Ca, p - 1)
      val Da = if (inversion._2 < 1)
        inversion._2 + p - 1
      else
        inversion._2
//      println(s"Debug: Ca = $Ca, Da = $Da, p - 1 = ${p - 1}")

      val Cb = gen_test_prime_number()
      inversion = gcd(Cb, p - 1)
      val Db = if (inversion._2 < 1)
        inversion._2 + p - 1
      else
        inversion._2
//      println(s"Debug: Cb = $Cb, Db = $Db, p - 1 = ${p - 1}")

      val x1 = FME(sym.toInt, Ca, p) // transfer from A to B

      val x2 = FME(x1, Cb, p) // transfer from B to A

      val x3 = FME(x2, Da, p) // transfer from A to B

      val x4 = FME(x3, Db, p)

      println(s"B abonent get ${x4.toChar} symbol")
    }
  }

  def El_Gamaal_cipher(m:String): Unit = {
    var pg = generateSophieGermain()

    var c1:BigInt = 0
    var c2:BigInt = 0
    c1 = gen_test_number(pg._1 - 1)
    c2 = gen_test_number(pg._1 - 1)

    val d1:BigInt = FME(pg._2, c1, pg._1)
    val d2:BigInt = FME(pg._2, c2, pg._1)

    for (sym <- m){
      val k = gen_test_number(pg._1 - 2)
      val r = FME(pg._2, k, pg._1) // counted by A
      /*
      d1, d2 - public keys
      c1, c2 - private keys
      */
      val e = (sym.toInt % pg._1 * FME(d2, k, pg._1)) % pg._1 // counted by A

      val o = (e % pg._1 * FME(r, pg._1 - 1 - c2, pg._1)) % pg._1 // counted by B
      println(s"Abonent B get ${o.toChar} symbol")
    }
  }

  def RSA(m:String): Unit ={
    val P1:BigInt = gen_test_prime_number()
    val Q1:BigInt = gen_test_prime_number()
    val N1:BigInt = P1 * Q1

    val fi1 = (P1 - 1) * (Q1 - 1)
    var f:Boolean = true
    var d1:BigInt = 0
    while (f) {
      d1 = gen_test_prime_number()
      if (gcd(d1, fi1)._1 == 1)
        f = false
    }
    var c1 = gcd(d1, fi1)._2
    if (c1 < 0) c1 += fi1
//    println(s"Debug: c1 = $c1, d1 = $d1, fi1 = $fi1")

    val P2:BigInt = gen_test_prime_number()
    val Q2:BigInt = gen_test_prime_number()
    val N2:BigInt = P2 * Q2

    val fi2 = (P2 - 1) * (Q2 - 1)
    f = true
    var d2:BigInt = 0
    while (f) {
      d2 = gen_test_prime_number()
      if (gcd(d2, fi2)._1 == 1)
        f = false
    }
    var c2 = gcd(d2, fi2)._2
    if (c2 < 0) c2 += fi2
//    println(s"Debug: c2 = $c2, d2 = $d2, fi2 = $fi2")

    var message:BigInt = 0

    for (sym <- m){
      message += sym.toInt
      message <<= 8
      val e = FME(sym.toLong, d2, N2)
      val decoded = FME(e, c2, N2)
      print(s"${decoded.toChar}")
    }
    println()
  }

}

