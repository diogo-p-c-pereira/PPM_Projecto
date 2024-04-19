package ZigZagGame

import ZigZagGame.ZigZag.Board
import ZigZagGame.ZigZag.Direction.Direction

import scala.annotation.tailrec

case class ZigZag(board: Board, rand: MyRandom/*, wordList: List[(String,Boolean)]*/){ }

object ZigZag {

  type Board = List[List[Char]]
  type Coord2D = (Int, Int) //(row, column)

  object Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West,
    NorthEast, NorthWest, SouthEast, SouthWest = Value

    def processCoord(coord: Coord2D, dir: Direction): Coord2D = dir match {
      case North => (coord._1 - 1, coord._2)
      case South => (coord._1 + 1, coord._2)
      case East => (coord._1, coord._2 + 1)
      case West => (coord._1, coord._2 - 1)
      case NorthEast => (coord._1 - 1, coord._2 + 1)
      case NorthWest => (coord._1 - 1, coord._2 - 1)
      case SouthEast => (coord._1 + 1, coord._2 + 1)
      case SouthWest => (coord._1 + 1, coord._2 - 1)
      case _ => coord
    }

    def getAllDirs(): List[Direction] = {
      Direction.values.toList
    }
  }

  val Empty = '%' //Para efeitos de vizualizacao de teste, podera ser alterado na versao final para ' '

  //T1
  def randomChar(rand: MyRandom): (Char, MyRandom) = {
    val i = rand.nextInt(26)
    ((i._1 + 'A').toChar, i._2)
  }

  //T2
  def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = {
    _foldFillOneCell(board, letter, coord)._1
    //_fillOneCell(board, letter, coord,0)
  }

  //using Fold
  def _foldFillOneCell(board: Board, letter: Char, coord: Coord2D): (Board, Int) = {
    def aux_foldFillOneCell(list: List[Char], letter: Char, coord: Int): (List[Char], Int) = {
      (list foldLeft(List[Char](), 0))((acc, e) => if (acc._2 == coord) (acc._1 :+ letter, acc._2 + 1) else (acc._1 :+ e, acc._2 + 1))
    }
    (board foldLeft(List[List[Char]](), 0))((acc, e) => if (acc._2 == coord._1) (acc._1 :+ aux_foldFillOneCell(e, letter, coord._2)._1, acc._2 + 1) else (acc._1 :+ e, acc._2 + 1))
  }

  //using PatternMatching
  def _fillOneCell(board: Board, letter: Char, coord: Coord2D, acc: Int): Board = {
    def aux_FillOneCell(list: List[Char], letter: Char, coord: Coord2D, acc: Int): List[Char] = list match {
      case Nil => Nil
      case x :: xs => if (acc == coord._2) letter :: xs else x :: aux_FillOneCell(xs, letter, coord, acc + 1)
    }
    board match {
      case Nil => List()
      case x :: xs => if (acc == coord._1) aux_FillOneCell(x, letter, coord, 0) :: xs else x :: _fillOneCell(xs, letter, coord, acc + 1)
    }
  }


  //T3
  @tailrec
  def setBoardWithWords(board: Board, words: List[String], positions: List[List[Coord2D]]): Board = {
    def aux_setBoardWithWords(board: Board, word: List[Char], position: List[Coord2D]): Board = word match {
      case Nil => board
      case x :: xs => aux_setBoardWithWords(fillOneCell(board, x, position.head), xs, position.tail)
    }
    words match {
      case Nil => board
      case x :: xs => setBoardWithWords(aux_setBoardWithWords(board, x.toList, positions.head), xs, positions.tail)
    }
  }


  //T4
  def completeBoardRandomly(board: Board, r: MyRandom, f: MyRandom => (Char, MyRandom)): (Board, MyRandom) = {
    def aux_completeBoardRandomly(list: List[Char], r: MyRandom, f: MyRandom => (Char, MyRandom)): (List[Char], MyRandom) = list match {
      case Nil => (Nil, r)
      case x :: xs => if (x == Empty) {
        val a = f(r);
        val b = aux_completeBoardRandomly(xs, a._2, f); (a._1 :: b._1, b._2)
      }
      else {
        val c = aux_completeBoardRandomly(xs, r, f); (x :: c._1, c._2)
      }
    }
    board match {
      case Nil => (List(), r)
      case x :: xs => val a = aux_completeBoardRandomly(x, r, f);
        val b = completeBoardRandomly(xs, a._2, f); (a._1 :: b._1, b._2)
    }
  }

  //T5 TODO Otimizar
  def play(board: Board, word: String, coord: Coord2D, dir: Direction): Boolean = {
    _play(board, word.toList, coord::List[Coord2D](), dir::List[Direction]())
  }

  def _play(board: Board, word: List[Char], coords: List[Coord2D], dirs: List[Direction]): Boolean = {
    def aux_play(board: Board, word: List[Char], coords: List[Coord2D], dir: Direction): Boolean = word match {
      case Nil => true
      case x :: xs => if (x == getChar(board, coords.last)) {
        val a = Direction.processCoord(coords.last, dir);
        if (verifyCoord(a, coords)) _play(board, xs, coords :+ a, Direction.getAllDirs()) else false }
      else false
    }
    dirs match {
      case Nil => false
      case x :: xs => aux_play(board, word, coords, x) || _play(board, word, coords, xs)
    }
  }



  def verifyCoord(coord2D: Coord2D, coords: List[Coord2D]): Boolean = coords match { //Verifica que não existe a coord2D na Lista
    case Nil => true
    case x :: xs => if (coord2D == x) false else verifyCoord(coord2D, xs)
  }

  def getChar(board: Board, coord: Coord2D): Char = { _getChar(board, coord,0) }

  def _getChar(board: Board, coord: Coord2D, acc: Int): Char = {
    def aux_getChar(list: List[Char], coord: Coord2D, acc: Int): Char = list match {
      case Nil => Empty
      case x::xs => if(acc == coord._2) x else aux_getChar(xs, coord, acc+1)
    }
    board match {
      case Nil => Empty
      case x::xs => if(acc == coord._1) aux_getChar(x, coord, 0) else _getChar(xs, coord, acc+1)
    }
  }


  //T8 Aux
  def initializeBoard(rowWidth: => Int, columnHeight: => Int, fileName: String)(zigZag: ZigZag): ZigZag = {
    val words = IO_Utils.loadWordsCoord(fileName)
    val b = setBoardWithWords(createBoard(rowWidth, columnHeight),words._1,words._2)
    val r = completeBoardRandomly(b, zigZag.rand, randomChar)
    new ZigZag(r._1, r._2/*, zigZag.wordList*/)
  }

  def createBoard(rowWidth: Int, columnHeight: Int): Board = {
      List.fill(columnHeight)(List.fill(rowWidth)(Empty))
  }

  def selectWord(word: => String, coord2D: => Coord2D, dir: => String)(zigZag: ZigZag): ZigZag = {
    if(play(zigZag.board, word, coord2D, matchDirection(dir))) {
      IO_Utils.printResult(true)
      //TODO função correr o atributo wordList e mudar para Verdadeiro
    } else {
      IO_Utils.printResult(false)
    }
    new ZigZag(zigZag.board, zigZag.rand)
  }

  def matchDirection(s: String): Direction = s match{
    case "NORTE" => Direction.North
    case "SUL" => Direction.South
    case "ESTE" => Direction.East
    case "OESTE" => Direction.West
    case "NORDESTE" => Direction.NorthEast
    case "NOROESTE" => Direction.NorthWest
    case "SUDESTE" => Direction.SouthEast
    case "SUDOESTE" => Direction.SouthWest
    case _ => null
  }

  //TODO
  /*def changeWordStatus(wordList: List[(String,Boolean)], word: String): List[(String,Boolean)] = wordList match {
    case Nil =>
  }*/

  def reset()(zigZag: ZigZag): ZigZag = {
    new ZigZag(List(List()),zigZag.rand/*, List()*/)
  }

  def exit(file: String)(zigZag: ZigZag): ZigZag = {
    IO_Utils.writeSeed(file, (zigZag.rand).seed)
    sys.exit()
  }

  def changeColor(color: => String)(zigZag: ZigZag): ZigZag = {
    IO_Utils.changeTextColor(color)
    zigZag
  }

  //Testes
  def main(args: Array[String]): Unit = {

    val board = List.fill(5)(List.fill(5)(Empty))
    val r = MyRandom(43326622)

    //Teste T1
    println("T1:")
    println(r)
    val rC = randomChar(r)
    println(rC)
    val rC2 = randomChar(rC._2)
    println(rC2)

    //Teste T2
    println("\nT2:")
    val board2 = fillOneCell(board, randomChar(MyRandom(10))._1 , (2,2))
    IO_Utils.printBoard(board2)

    //Teste T3
    println("\nT3:")
    val board3 = setBoardWithWords(board, List("DIOGO","AI"), List(List((1,0),(1,1),(1,2),(2,2),(1,3)),List((4,3),(3,4))))
    IO_Utils.printBoard(board3)

    //Teste T4
    println("\nT4:")
    val board4 = completeBoardRandomly(board3, r, randomChar)._1
    IO_Utils.printBoard(board4)

    //Teste T5
    println()
    val board5 = setBoardWithWords(board, List("DIOG","AI"), List(List((1,0),(1,1),(1,2),(2,2)),List((4,3),(3,4))))
    val board6 = completeBoardRandomly(board5, r, randomChar)._1
    IO_Utils.printBoard(board6)
    //println(getChar(board6,(2,2)))
    println()
    println(play(board4,"DIOGO",(1,0), Direction.East))
  }

}
