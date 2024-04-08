package ZigZagGame

import ZigZagGame.ZigZag.Board
import ZigZagGame.ZigZag.Direction.Direction

import scala.annotation.tailrec

case class ZigZag(board: Board, rand: MyRandom){ }

object ZigZag {

  type Board = List[List[Char]]
  type Coord2D = (Int, Int) //(row, column)
  object Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West,
    NorthEast, NorthWest, SouthEast, SouthWest = Value
  }

  val Empty = '%' //Para efeitos de vizualizacao de teste, podera ser alterado na versao final para ' '

  //T1
  def randomChar(rand: MyRandom): (Char, MyRandom) = {
    val i = rand.nextInt(26)
    ((i._1+'A').toChar, i._2)
  }

  //T2
  def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = { _foldFillOneCell(board, letter, coord)._1 }

  def _foldFillOneCell(board: Board, letter: Char, coord: Coord2D): (Board,Int) = {
    (board foldLeft (List[List[Char]](),0)) ((acc,e) => if(acc._2 == coord._1) (acc._1:+__foldFillOneCell(e, letter, coord._2)._1, acc._2+1) else (acc._1:+e , acc._2+1))
  }

  def __foldFillOneCell(list: List[Char], letter: Char, coord: Int): (List[Char],Int) = {
    (list foldLeft (List[Char](),0)) ((acc,e) => if(acc._2 == coord) (acc._1:+letter, acc._2+1) else (acc._1:+e, acc._2+1))
  }

  /*def _fillOneCell(board: Board, letter: Char, coord: Coord2D, acc: Int): Board = board match {
    case Nil =>  List()
    case x::xs => if(acc == coord._1) __FillOneCell(x, letter, coord, 0)::xs else x::_fillOneCell(xs, letter, coord, acc+1)
  }

  def __FillOneCell(list: List[Char], letter: Char, coord: Coord2D, acc: Int): List[Char] = list match {
    case Nil => Nil
    case x::xs => if(acc == coord._2) letter::xs else x::aux_FillOneCell(xs, letter, coord, acc+1)
  }*/

  //T3
  @tailrec
  def setBoardWithWords(board:Board, words:List[String], positions:List[List[Coord2D]]): Board = words match{
    case Nil => board
    case x::xs => setBoardWithWords(aux_setBoardWithWords(board, x.toList, positions.head), xs, positions.tail)
  }

  @tailrec
  def aux_setBoardWithWords(board:Board, word: List[Char], position: List[Coord2D]): Board = word match{
    case Nil => board
    case x::xs => aux_setBoardWithWords(fillOneCell(board, x, position.head), xs, position.tail)
  }

  //T4
  def completeBoardRandomly(board: Board, r: MyRandom, f: MyRandom => (Char, MyRandom)): (Board, MyRandom) = board match{
    case Nil => (List(),r)
    case x::xs => val a = aux_completeBoardRandomly(x,r,f); val b = completeBoardRandomly(xs,a._2,f); (a._1::b._1, b._2)
  }

  def aux_completeBoardRandomly(list: List[Char], r: MyRandom, f: MyRandom => (Char, MyRandom)): (List[Char], MyRandom) = list match{
    case Nil => (Nil,r)
    case x::xs => if(x==Empty) { val a = f(r); val b = aux_completeBoardRandomly(xs,a._2,f) ; (a._1::b._1, b._2) }
                    else { val c = aux_completeBoardRandomly(xs,r,f); (x::c._1, c._2) }
  }

  //TODO T5
  def play(word: String, coord: Coord2D, dir: Direction): Boolean = { //ou ZigZag???
    true
  }



  //T8 Aux
  def initializeBoard(rowWidth: => Int, columnHeight: => Int, fileName: String)(zigZag: ZigZag): ZigZag = {
    val words = IO_Utils.loadWordsCoord(fileName)
    val b = setBoardWithWords(createBoard(rowWidth, columnHeight),words._1,words._2)
    val r = completeBoardRandomly(b, zigZag.rand, randomChar)
    new ZigZag(r._1, r._2)
  }

  def createBoard(rowWidth: Int, columnHeight: Int): Board = {
      List.fill(columnHeight)(List.fill(rowWidth)(Empty))
  }

  def reset()(zigZag: ZigZag): ZigZag = {
    new ZigZag(List(List()),zigZag.rand)
  }

  def exit(file: String)(zigZag: ZigZag): ZigZag = {
    IO_Utils.writeSeed(file, (zigZag.rand).seed)
    sys.exit()
  }

  def changeColor(color: => String)(zigZag: ZigZag): ZigZag = {
    //IO_Utils.changeTextColor(color)
    zigZag
  }

  //Testes
  def main(args: Array[String]): Unit = {

    val board = List.fill(5)(List.fill(5)(Empty))
    val r = MyRandom(10)

    //Teste T1
    /*val rand = randomChar(r)
    print(rand)
    val r2 = randomChar(rand._2)
    print(r2)*/

    //Teste T2
    /*val board2 = fillOneCell(board, randomChar(MyRandom(10))._1 , (2,2))
    IO_Utils.printBoard(board2)*/

    //Teste T3
    /*val board2 = setBoardWithWords(board, List("DIOGO","AO"), List(List((1,0),(1,1),(1,2),(1,3),(1,4)),List((4,3),(3,4))))
    IO_Utils.printBoard(board2)*/

    //Teste T4
    val board2 = setBoardWithWords(board, List("DIOGO","AO"), List(List((1,0),(1,1),(1,2),(1,3),(1,4)),List((4,3),(3,4))))
    val board3 = completeBoardRandomly(board2, r, randomChar)._1
    IO_Utils.printBoard(board2)

    //Teste T5
    //TODO


  }

}
