package ZigZagGame

case class ZigZag(rand: MyRandom){ }

object ZigZag {

  type Board = List[List[Char]]
  type Coord2D = (Int, Int) //(row, column)
  object Direction extends Enumeration {
    type Direction = Value
    val North, South, East, West,
    NorthEast, NorthWest, SouthEast, SouthWest = Value
  }

  val row = List.fill(10)('x')
  val board = List.fill(10)(row)

  //T1
  def randomChar(rand: MyRandom): (Char, MyRandom) = {
    val i = rand.nextInt(26)
    ((i._1+65).toChar, i._2)
  }


  //T2
  def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = { _fillOneCell(board, letter, coord, 0) }

  def _fillOneCell(board: Board, letter: Char, coord: Coord2D, acc: Int): Board = board match {
    case Nil =>  List()
    case x::xs => if(acc == coord._1) aux_FillOneCell(x, letter, coord, 0)::xs else x::_fillOneCell(xs, letter, coord, acc+1)
  }

  def aux_FillOneCell(list: List[Char], letter: Char, coord: Coord2D, acc: Int): List[Char] = list match {
    case Nil => Nil
    case x::xs => if(acc == coord._2) letter::xs else x::aux_FillOneCell(xs, letter, coord, acc+1)
  }

  //T3
  def setBoardWithWords(board:Board, words:List[String], positions:List[List[Coord2D]]): Board = words match{
    case x::Nil => aux_setBoardWithWords(board, x.toList, positions.head)
    case x::xs => setBoardWithWords(aux_setBoardWithWords(board, x.toList, positions.head), xs, positions.tail)
  }

  def aux_setBoardWithWords(board:Board, word: List[Char], position: List[Coord2D]): Board = word match{
    case x::Nil => fillOneCell(board, x, position.head)
    case x::xs => aux_setBoardWithWords(fillOneCell(board, x, position.head), xs, position.tail)
  }

  //T4
  def completeBoardRandomly(board: Board, r: MyRandom, f: MyRandom => (Char, MyRandom)): (Board, MyRandom) = { (board,r) }

  def main(args: Array[String]): Unit = {

    //Teste T1
    /*val r = MyRandom(9)
    val rand = randomChar(r)
    print(rand)
    val r2 = randomChar(rand._2)
    print(r2)*/

    //Teste T2
    /*val board2 = fillOneCell(board, randomChar(MyRandom(10))._1 , (9,9))
    IO_Utils.printBoard(board2)*/

    //Teste T3
    val board2 = setBoardWithWords(board, List("DIOGO","AO"), List(List((1,0),(1,1),(1,2),(1,3),(1,4)),List((4,4),(3,5))))
    IO_Utils.printBoard(board2)

  }

}
