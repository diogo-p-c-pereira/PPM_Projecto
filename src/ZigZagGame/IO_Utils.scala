package ZigZagGame

import java.io._
import scala.collection.SortedMap
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

object IO_Utils {

  def getUserInputInt(msg: String): Try[Int] = {
    print(msg + ": ")
    Try(readLine.trim.toUpperCase.toInt)
  }

  def prompt(msg: String): String = {
    print(msg + ": ")
    scala.io.StdIn.readLine()
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

}
