package ZigZagGame

import java.io._
import scala.collection.SortedMap
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}
import scala.Console

object IO_Utils {

  def getUserInputInt(msg: String): Try[Int] = {
    print(msg + ": ")
    Try(readLine.trim.toUpperCase.toInt)
  }

  def prompt(msg: String): String = {
    print(msg + ": ")
    scala.io.StdIn.readLine().toUpperCase
  }

  def optionPrompt(options: SortedMap[Int, CommandLineOption]): Option[CommandLineOption] = {
    println("-- Options --")
    options.toList map
      ((option: (Int, CommandLineOption)) => println(option._1 + ") " + option._2.name))

    getUserInputInt("Select an option") match {
      case Success(i) => options.get(i)
      case Failure(_) => println("Invalid number!"); optionPrompt(options)
    }
  }

  /*def printBoard[A](board: List[List[A]]): Unit = board match {
    case Nil => Nil
    //case x::xs => printRow(x); printBoard(xs)
    case x::xs => printRowMap(x); println(); printBoard(xs)
  }

  def printRow[A](list: List[A]): Unit = list match {
    case Nil => println()
    case x::xs => print(x + " "); printRow(xs)
  }*/

  def printBoard[A](board: List[List[A]]): Unit = {
    board.map(x => printRow(x))
  }

  def printRow[A](list: List[A]): Unit = {
    list.map(x => print(x + " "))
    println()
  }

  def loadSeed(file: String) = {
    val bufferedSource = Source.fromFile(file)
    val a = bufferedSource.getLines.mkString
    bufferedSource.close
    a.toLong
  }

  def writeSeed(file: String, seed: Long) = {
    val pw = new PrintWriter(new File(file))
    pw.write(seed.toString)
    pw.close
  }

  def loadWordsCoord(file: String): (List[String],List[List[(Int,Int)]]) = {
    val bufferedSource = Source.fromFile(file)
    var a = List[String]()
    var b = List[List[(Int,Int)]]()
    for (line <- bufferedSource.getLines){
      val d = line.toUpperCase.mkString.split(";")
      val p = d(1).split(" ")
      var s = List[(Int,Int)]()
      for(m <- p){
        val z = m.split(",")
        s = s:+(z(0).toInt,z(1).toInt)
      }
      a= a:+d(0)
      b= b:+s
    }
    bufferedSource.close
    (a,b)
  }

  def changeTextColor(color: String) = color match {
    case "PRETO" => print(Console.BLACK)
    case "AZUL" => print(Console.BLUE)
    case "VERDE" => print(Console.GREEN)
    case "VERMELHO" => print(Console.RED)
    case "BRANCO" => print(Console.RESET)
    case "AMARELO" => print(Console.YELLOW)
    case _ => println("Cor Invalida")
  }

  //Testes
  def main(args: Array[String]): Unit = {
    println(loadSeed("seed.txt"))
    println(loadWordsCoord("words.txt"))
  }

}
