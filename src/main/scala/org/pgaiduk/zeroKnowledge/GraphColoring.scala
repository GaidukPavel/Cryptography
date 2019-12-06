package org.pgaiduk.zeroKnowledge

import org.pgaiduk.Cryptography.{Crypto, Crypto_ciphers}

import scala.io.Source
import scala.util.Random

class GraphColoring {
  /* Constructor to read and write graph to a structure*/
  var graph:List[((Int, Int), List[Int])] = List[((Int, Int), List[Int])]()
  val filename = "resources/graph_main"
  for (line <- Source.fromFile(filename).getLines()){
    var vertex:(Int, Int) = (line.split("#")(0).split(",")(0).toInt, line.split("#")(0).split(",")(1).toInt)
    var adjacency_list:List[Int] = line.split("#")(1).split(",").toList.map(_.toInt)
    graph ::= (vertex, adjacency_list)
  }
  graph = graph.reverse
  /***********/
  var graph_links: List[(Int, Int)] = translate_to_vert_list()
  var vertexes_colors: List[(Int, Int /* Color */)] = {
    var vert_tmp:List[(Int /*vertex number*/,Int /* Color */ )] = List[(Int, Int)]()
    for (vertex <- graph){
      vert_tmp ::=  vertex._1
    }
    vert_tmp
  }
  vertexes_colors = vertexes_colors.reverse
  var graph_changed:List[(BigInt, BigInt)] = List[(BigInt, BigInt)]()
  var list_vertex_nums:Array[Int] = new Array[Int](vertexes_colors.length)
  var list_vertex_RSA:Array[(BigInt /*d*/, BigInt /*N*/, BigInt /*c*/)] = new Array[(BigInt /*d*/, BigInt /*N*/, BigInt /*c*/)](vertexes_colors.length)

  changeGraph()

  private def changeGraph(): Unit = {
    var colors:List[Int] = 1 :: 2 :: 3 :: Nil
    do {
      colors = Random.shuffle(colors)
    } while (List(1, 2, 3) == colors)
    for (vertex <- vertexes_colors) {
      var vertex_number:Int = Crypto.gen_number(30).toInt
      vertex_number >>= 2
      vertex_number = (vertex_number << 2) + colors(vertex._2 % 3)
      list_vertex_nums(vertex._1) = vertex_number
      list_vertex_RSA(vertex._1) = Crypto_ciphers.RSA_generate_keys()
    }
    for (link <- graph_links){
      graph_changed ::= (Crypto_ciphers.RSA_encrypt(list_vertex_nums(link._1), list_vertex_RSA(link._1)._1, list_vertex_RSA(link._1)._2), Crypto_ciphers.RSA_encrypt(list_vertex_nums(link._2), list_vertex_RSA(link._2)._1, list_vertex_RSA(link._2)._2))
    }
    graph_changed = graph_changed.reverse
  }

  def checkLink(link_num:Int):Boolean = {
    val first_vertex_color = Crypto_ciphers.RSA_decrypt(graph_changed(link_num)._1, list_vertex_RSA(graph_links(link_num)._1)._3, list_vertex_RSA(graph_links(link_num)._1)._2) & 3
    val second_vertex_color = Crypto_ciphers.RSA_decrypt(graph_changed(link_num)._2,  list_vertex_RSA(graph_links(link_num)._2)._3, list_vertex_RSA(graph_links(link_num)._2)._2) & 3
    if (first_vertex_color != second_vertex_color)
      return true
    false
  }

  def get_number_of_links():Int = {
    graph_links.length
  }

  private def translate_to_vert_list(): List[(Int, Int)] = {
    var converted:List[(Int, Int)] = List[(Int, Int)]()
    for (line <- graph) {
      for (elem <- line._2) {
        if (line._1._1 != elem) {
          if (converted.contains((line._1._1, elem)) || converted.contains((elem, line._1._1))) {

          }
          else
          {
            converted ::= (line._1._1, elem)
          }
        }
      }
    }
    converted.reverse
  }

  def print_graph(): Unit ={
    println("Links:")
    for (link <- graph_links){
      println(s"(${link._1}, ${link._2})")
    }
    println("Vertexes colors:")
    for (vertex <- vertexes_colors){
      println(s"(${vertex._1}, ${vertex._2})")
    }
  }

}
