package org.pgaiduk.zeroKnowledge

import org.pgaiduk.Cryptography.{Crypto, Crypto_ciphers}

import scala.io.Source
import scala.util.Random

class GraphColoring {
  /* Constructor to read and write graph to a structure*/
  var graph:List[((Int, Int), List[Int])] = List[((Int, Int), List[Int])]()
  val filename = "resources/graph2.txt"
  for (line <- Source.fromFile(filename).getLines()){
    var vertex:(Int, Int) = (line.split("#")(0).split(",")(0).toInt, line.split("#")(0).split(",")(1).toInt)
    var adjacency_list:List[Int] = line.split("#")(1).split(",").toList.map(_.toInt)
    graph ::= (vertex, adjacency_list)
  }
  graph = graph.reverse
  println("graph was:")
  for (vertex <- graph) {
    println(s"vertex number = ${vertex._1._1} color = ${vertex._1._2}")
  }
  /***********/
  var graph_changed:List[((Int, Int), List[Int])] = List[((Int, Int), List[Int])]()

  private def changeGraph(): Unit = {
    var colors:List[Int] = 1 :: 2 :: 3 :: Nil
    colors = Random.shuffle(colors)
    for (vertex <- graph) {
      var vertex_number:Int = Crypto.gen_number(30).toInt
      vertex_number >>= 2
      vertex_number = (vertex_number << 2) + colors(vertex._1._2 % 3)
      graph_changed ::= ((vertex_number, colors(vertex._1._2 % 3)), vertex._2)
    }
    graph_changed = graph_changed.reverse
    println("graph now:")
    for (vertex <- graph_changed) {
      println(s"vertex number = ${vertex._1._1.toBinaryString} color = ${vertex._1._2}")
    }
  }
  var RSA_data:List[(BigInt /*d*/, BigInt /*N*/, BigInt /*c*/)] = List[(BigInt, BigInt, BigInt)]()
  private def generateRSAData(): Unit = {
    for (i <- graph_changed.indices){
      RSA_data ::= Crypto_ciphers.RSA_generate_keys()
    }
  }
  changeGraph()
  generateRSAData()
  RSA_data = RSA_data.reverse
  var encrypted:List[(BigInt /*d*/, BigInt /*N*/, BigInt /*Z*/)] = List[(BigInt, BigInt, BigInt)]() // this data is for Bob
  private def encrypt(): Unit = {
    for (i <- graph_changed.indices) {
      encrypted ::= (RSA_data(i)._1, RSA_data(i)._2, Crypto_ciphers.RSA_encrypt(graph_changed(i)._1._1, RSA_data(i)._1, RSA_data(i)._2))
    }
  }
  encrypt()
  encrypted = encrypted.reverse
  def checkLink(s:Int, e:Int):Boolean = {
    val c1:Int = (Crypto_ciphers.RSA_decrypt(encrypted(s-1)._3, RSA_data(s-1)._3, encrypted(s-1)._2) & 3).toInt
    val c2:Int = (Crypto_ciphers.RSA_decrypt(encrypted(e-1)._3, RSA_data(e-1)._3, encrypted(e-1)._2) & 3).toInt
    println(s"c1 = ${Crypto_ciphers.RSA_decrypt(encrypted(s-1)._3, RSA_data(s-1)._3, encrypted(s-1)._2).toBinaryString}")
    println(s"c2 = ${Crypto_ciphers.RSA_decrypt(encrypted(e-1)._3, RSA_data(e-1)._3, encrypted(e-1)._2).toBinaryString}")
    println(s"colors = $c1, $c2")
    if (c1 != c2)
      true
    else
      false
  }

}
