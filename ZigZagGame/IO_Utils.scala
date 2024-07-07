package ZigZagGame

import ZigZagGame.Direction.Direction

import java.io._
import java.text.SimpleDateFormat
import java.util.Calendar
import scala.collection.SortedMap
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}
import scala.Console
import scala.annotation.tailrec

object IO_Utils {

  def getUserInputInt(msg: String): Try[Int] = {
    print(msg + ": ")
    Try(readLine.trim.toUpperCase.toInt)
  }

  def prompt(msg: String): String = {
    print(msg + ": ")
    scala.io.StdIn.readLine().toUpperCase
  }

  @tailrec
  def optionPrompt(options: SortedMap[Int, CommandLineOption]): Option[CommandLineOption] = {
    println("-- Options --")
    options.toList map
      ((option: (Int, CommandLineOption)) => println(option._1 + ") " + option._2.name))

    getUserInputInt("Select an option") match {
      case Success(i) => options.get(i)
      case Failure(_) => println("Invalid number!"); optionPrompt(options)
    }
  }

  def filePrompt(): String = {
    @tailrec
    def printFiles(list: List[File], i: Int): Unit = list match {
      case Nil =>
      case x :: xs => println((i + 1) + ")" + x.toString); printFiles(xs, i + 1)
    }

    println("-- SAVE FILES --")
    val files = new File(".").listFiles(new FileFilter {
      override def accept(pathname: File): Boolean = pathname.getName.endsWith(".save")
    }).toList
    printFiles(files, 0)
    getUserInputInt("Select a file") match {
      case Success(i) => if(i<files.size+1) files(i - 1).getName else {printInvalidOption(); null}
      case Failure(_) => println("Invalid number!"); null
    }
  }

  def dirPrompt(): Direction = {
    @tailrec
    def printDirs(list: List[Direction], i: Int): Unit = list match {
      case Nil =>
      case x :: xs => println((i + 1) + ")" + x.toString); printDirs(xs, i + 1)
    }

    println("-- Directions --")
    val dirs = Direction.getAllDirs
    printDirs(dirs, 0)
    getUserInputInt("Select a direction") match {
      case Success(i) => if(i<dirs.size+1) dirs(i - 1) else {printInvalidOption(); null}
      case Failure(_) => println("Invalid number!"); null
    }
  }

  def printBoard(board: ZigZag.Board, coords: List[ZigZag.Coord2D]): Unit = {
    @tailrec
    def _printBoard(board: ZigZag.Board, coords: List[ZigZag.Coord2D], acc: Int): Unit = {
      @tailrec
      def printRow(list: List[Char], coords: List[ZigZag.Coord2D], i: Int, j: Int): Unit = {
        @tailrec
        def isCorrect(i: Int, j: Int, coords: List[ZigZag.Coord2D]): Boolean = coords match{
          case Nil => false
          case x::xs => if(x._1==i && x._2==j) true else isCorrect(i, j, xs)
        }
        list match {
          case Nil => println()
          case x :: xs => if(isCorrect(i,j, coords)){ print(Console.GREEN + x + " " + Console.RESET);}
                          else { print(x + " "); }
                          printRow(xs, coords,i, j+1)
        }
      }
      board match {
        case Nil =>
        case x :: xs => printRow(x, coords, acc,0); _printBoard(xs, coords, acc+1)
      }
    }
    _printBoard(board, coords,0)
  }

  def printWordsFound(found: Int, n:Int): Unit = {
    println("\nWords found: " + found + "/" + n)
  }

  def printBoard[A](board: List[List[A]]): Unit = {
    board.map(x => printRow(x))
  }

  def printRow[A](list: List[A]): Unit = {
    list.map(x => print(x + " "))
    println()
  }

  //loads a seed from a file
  def loadSeed(file: String): Long = {
    val a = Try {
      val bufferedSource = Source.fromFile(file)
      val b = bufferedSource.getLines.mkString
      bufferedSource.close
      b.toLong
    }
    a match { //Case the file doesn't, aka first run or deleted for some reason, generates a new seed witb the current time and current pid
      case Success(_) => a.get
      case Failure(_) => System.currentTimeMillis() * ProcessHandle.current.pid
    }
  }

  //writes the seed to a file
  def writeSeed(file: String, seed: Long): Unit = {
    val pw = new PrintWriter(new File(file))
    pw.write(seed.toString)
    pw.close
  }

  //is called to print if checkboard returns false, receives my random case the issue is caused by a random letter
  def errorBoard(): Unit = {
    println(Console.RED + "Error on initializing Board, verify file or try again" + Console.RESET)
  }

  def printInvalidOption(): Unit = {
    println(Console.RED + "Invalid option" + Console.RESET)
  }

  //loads the word list from a file
  def loadWordsCoord(file: String): (List[String], List[List[(Int, Int)]]) = {
    val bufferedSource = Source.fromFile(file)
    var words = List[String]()
    var coords = List[List[(Int, Int)]]()
    for (line <- bufferedSource.getLines) {
      val a = line.toUpperCase.mkString.split(";")
      val b = a(1).split(" ")
      var c = List[(Int, Int)]()
      for (t <- b) {
        val e = t.split(",")
        c = c :+ (e(0).toInt, e(1).toInt)
      }
      words = words :+ a(0)
      coords = coords :+ c
    }
    bufferedSource.close
    (words, coords)
  }

  //Loads game state from binary class
  def loadGame(file: String): ZigZag = {
    var in = None: Option[FileInputStream]
    try{
      in = Some(new FileInputStream(file))
      val obj = Some(new ObjectInputStream(in.get))
      obj.get.readObject().asInstanceOf[ZigZag]
    } catch {
      case e: IOException => e.printStackTrace; sys.exit()
    } finally {
      if (in.isDefined) in.get.close
    }
  }

  //Saves game state to Binary File
  def saveGame(zigZag: ZigZag): Unit = {
    var out = None: Option[FileOutputStream]
    try {
      val format = new SimpleDateFormat("yyyy-MM-dd HH_mm_ss")
      val now = format.format(Calendar.getInstance().getTime)
      out = Some(new FileOutputStream(now + ".save"))
      val obj = Some(new ObjectOutputStream(out.get))
      obj.get.writeObject(zigZag)
    } catch {
      case e: IOException => e.printStackTrace
    } finally {
      if (out.isDefined) out.get.close
    }
  }


  //prints the play result
  def printResult(boolean: Boolean): Unit = {
    if(boolean){
      println(Console.BLUE + "Acertou!" + Console.RESET)
    }else{
      println(Console.RED + "Errou!" + Console.RESET)
    }
  }

  //prints the game result
  def printGameCleared(score: Long, time: Long): Unit = {
    println(Console.BLUE + "Concluido!!!" + Console.RESET)
    println("Score: " + score)
    println("Time: " + String.format("%02d", time/60) + "m" + String.format("%02d", time%60) + "s")
  }

  //Testes
  def main(args: Array[String]): Unit = {
    println(loadSeed("seed.txt"))
    println(loadWordsCoord("words.txt"))
  }

}
