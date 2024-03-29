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


  //T1
  def randomChar(rand: MyRandom): (Char, MyRandom) = {
    val i = rand.nextInt(26)
    ((i._1+65).toChar, i._2)
  }

  //T2
  def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = { board }

  //T3
  def setBoardWithWords(board:Board, words:List[String], positions:List[List[Coord2D]]): Board = { board }

  //T4
  def completeBoardRandomly(board: Board, r: MyRandom, f: MyRandom => (Char, MyRandom)): (Board, MyRandom) = { (board,r) }

  def main(args: Array[String]): Unit = {
    val r = MyRandom(9)
    val rand = randomChar(r)
    print(rand)
    val r2 = randomChar(rand._2)
    print(r2)
  }

}
