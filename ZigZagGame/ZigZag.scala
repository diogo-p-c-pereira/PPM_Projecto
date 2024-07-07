package ZigZagGame

import ZigZagGame.ZigZag.{Board, Coord2D}
import ZigZagGame.Direction.{Direction, getAllDirs}

import scala.annotation.{tailrec, unused}

case class ZigZag(board: Board, rand: MyRandom, wordList: (List[(String,Boolean)],List[ZigZag.Coord2D]), time: Long, failedPlays: Int){
  val serialVersionUID = 6529685098267757690L

  //GUI Support
  def initializeBoard(rowWidth: Int, columnHeight: Int, fileName: String): ZigZag = ZigZag.initializeBoard(rowWidth, columnHeight,fileName)(this)
  def reset(): ZigZag = ZigZag.reset()(this)
  def exit(fileName: String): ZigZag = ZigZag.exit(fileName)(this)
  def save(): ZigZag = ZigZag.save()(this)
  def load(file: String): ZigZag = ZigZag.load(file)(this)
  def incrementFailedPlays(): ZigZag = new ZigZag(this.board, this.rand, this.wordList, this.time, this.failedPlays+1)
  def play(word: String, coord: Coord2D, dir: Direction): (Boolean,List[Coord2D]) = ZigZag.play(this.board, word, coord, dir)
  //def playGUI(word: String, coords: List[ZigZag.Coord2D]): Boolean = ZigZag.playGUI(word, coords)(this)
  def selectWord(word: String, coords: List[Coord2D]): ZigZag = ZigZag.selectWord(word,coords)(this)
  def isGameCleared: Boolean = ZigZag.isGameCleared(this.wordList._1)
  def howManyWordsFound(): Int = ZigZag.howManyWordsFound(this.wordList._1)
  def isInWordList(word: String): Boolean = ZigZag.isInWordList(word, this.wordList._1)
  def calculateScore(time: Long): Long = ZigZag.calculateScore(time, this.failedPlays)
}

object ZigZag {

  type Board = List[List[Char]]
  type Coord2D = (Int, Int) //(row, column)

  private val Empty = '%' //For testing porposes, may be changed in the final version to ' '

  //T1
  def randomChar(rand: MyRandom): (Char, MyRandom) = {
    val i = rand.nextInt(26)
    ((i._1 + 'A').toChar, i._2)
  }

  //T2
  def fillOneCell(board: Board, letter: Char, coord: Coord2D): Board = {
    //using Fold
    def _foldFillOneCell(board: Board, letter: Char, coord: Coord2D): (Board, Int) = {
      def aux_foldFillOneCell(list: List[Char], letter: Char, coord: Int): (List[Char], Int) = {
        (list foldLeft(List[Char](), 0))((acc, e) => if (acc._2 == coord) (acc._1 :+ letter, acc._2 + 1) else (acc._1 :+ e, acc._2 + 1))
      }
      (board foldLeft(List[List[Char]](), 0))((acc, e) => if (acc._2 == coord._1) (acc._1 :+ aux_foldFillOneCell(e, letter, coord._2)._1, acc._2 + 1) else (acc._1 :+ e, acc._2 + 1))
    }
    //using PatternMatching
    @unused
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

    //_fillOneCell(board, letter, coord,0)
    _foldFillOneCell(board, letter, coord)._1
  }

  //T3
  @tailrec
  def setBoardWithWords(board: Board, words: List[String], positions: List[List[Coord2D]]): Board = {
    @tailrec
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
        val a = f(r)
        val b = aux_completeBoardRandomly(xs, a._2, f); (a._1 :: b._1, b._2)
      }
      else {
        val c = aux_completeBoardRandomly(xs, r, f); (x :: c._1, c._2)
      }
    }
    board match {
      case Nil => (List(), r)
      case x :: xs => val a = aux_completeBoardRandomly(x, r, f)
        val b = completeBoardRandomly(xs, a._2, f); (a._1 :: b._1, b._2)
    }
  }

  //Optimized T3 and T4 (and also substitutes T2)
  def setBoard(board: Board, r:MyRandom,f: MyRandom => (Char, MyRandom), words: List[String], positions: List[List[Coord2D]]): (Board, MyRandom) = {
    def _setBoard(board: Board, r:MyRandom,f: MyRandom => (Char, MyRandom), words: List[String], positions: List[List[Coord2D]], acc: Int): (Board, MyRandom) = {
      def aux_setBoard(list: List[Char], r:MyRandom,f: MyRandom => (Char, MyRandom), chars: List[(Char,Coord2D)], acc: Int): (List[Char], MyRandom) = {
        def searchCharWihtIndex(chars: List[(Char,Coord2D)], i: Int): Char = {
          (chars foldRight Empty)((e,acc)=> if(e._2._2 == i) e._1 else acc)
        }

        list match {
          case Nil => (Nil,r)
          case _::xs => val letter = searchCharWihtIndex(chars, acc)
            if(letter != Empty) {
              val a = aux_setBoard(xs,r,f,chars,acc+1)
              (letter::a._1,a._2)
            } else {
              val b = f(r)
              val c = aux_setBoard(xs,b._2,f,chars,acc+1)
              (b._1::c._1, c._2)
            }
        }
      }

      def searchRow(words: List[String], positions: List[List[Coord2D]], line: Int): List[(Char,Coord2D)] = { //Filters letters to the letters that are on that row of the board
        def _searchRow(list: List[Char], pos: List[Coord2D], line: Int): List[(Char,Coord2D)] = pos match {
          case Nil => Nil
          case x::xs =>  if(x._1==line){ val a = _searchRow(list.tail, xs, line)
            (list.head,x)::a } else { _searchRow(list.tail, xs, line) }
        }
        positions match{
          case Nil => Nil
          case x::xs => val a = _searchRow(words.head.toList, x, line)
            val b = searchRow(words.tail, xs, line)
            a:::b
        }
      }

      board match {
        case Nil => (Nil, r)
        case x :: xs => val a = aux_setBoard(x, r, f, searchRow(words, positions, acc), 0)
          val b =_setBoard(xs, a._2, f, words, positions, acc+1)
          (a._1::b._1 , b._2)
      }
    }
    _setBoard(board, r, f, words, positions, 0)
  }



  //Original T5 without returning coords
  /*def play(board: Board, word: String, coord: Coord2D, dir: Direction): Boolean = {
    _play(board, word.toList, coord::List[Coord2D](), dir::List[Direction]())
  }

  def _play(board: Board, word: List[Char], coords: List[Coord2D], dirs: List[Direction]): Boolean = {
    def aux_play(board: Board, word: List[Char], coords: List[Coord2D], dir: Direction): Boolean = word match {
      case Nil => true
      case x :: xs => if (x == getChar(board, coords.last)) {
        val a = Direction.processCoord(coords.last, dir)
        if (verifyCoord(a, coords)) _play(board, xs, coords :+ a, Direction.getAllDirs()) else false }
      else false
    }
    (dirs foldRight false)((e,acc) => aux_play(board, word, coords, e) || acc)
  }*/

  //T5 Modified to return correct coords
  def play(board: Board, word: String, coord: Coord2D, dir: Direction): (Boolean,List[Coord2D]) = {
    _play(board, word.toList, coord::List[Coord2D](), dir::List[Direction]())
  }

  //is not nested because it is also used by T6
  def _play(board: Board, word: List[Char], coords: List[Coord2D], dirs: List[Direction]): (Boolean,List[Coord2D]) = {
    def aux_play(board: Board, word2: List[Char], coords2: List[Coord2D], dir: Direction): (Boolean,List[Coord2D]) = word2 match {
      case Nil => (true,coords2)
      case x::Nil => if (x == getChar(board, coords2.last)) (true,coords2)  else (false,Nil)
      case x :: xs => if (x == getChar(board, coords2.last)) {
        val a = Direction.processCoord(coords2.last, dir)
        if (verifyCoord(a, coords2))  _play(board, xs, coords2 :+ a, Direction.getAllDirs) else (false,Nil) }
      else (false,Nil)
    }
    //Verifies if a Coord2D exists in a list
    def verifyCoord(coord2D: Coord2D, coords: List[Coord2D]): Boolean = {
      (coords foldRight true)((e,acc)=> if(e==coord2D) false else acc)
    }

    dirs match {
      case Nil => (false,coords)
      case x :: xs => val a = aux_play(board, word, coords, x)
        val b = _play(board, word, coords, xs)
        if(a._1) {
          (a._1||b._1, a._2)
        }else{
          (a._1||b._1, b._2)
        }
    }
  }

  //Used by plau to eturn the char of a specified Coordinate, not nested because it was also used in playGUI
  def getChar(board: Board, coord: Coord2D): Char = {
    @tailrec
    def _getChar(board: Board, coord: Coord2D, acc: Int): Char = {
      @tailrec
      def aux_getChar(list: List[Char], coord: Coord2D, acc: Int): Char = list match {
        case Nil => Empty
        case x::xs => if(acc == coord._2) x else aux_getChar(xs, coord, acc+1)
      }
      board match {
        case Nil => Empty
        case x::xs => if(acc == coord._1) aux_getChar(x, coord, 0) else _getChar(xs, coord, acc+1)
      }
    }
    _getChar(board, coord,0)
  }

  //OLD T6, does not verify word bifurcations
  /*def checkBoard(ogBoard: Board, wordList: List[String]): Boolean = {
    def _checkBoard(board: Board, wordList: List[(String,Boolean)], acc: Int): (Boolean, List[(String, Boolean)]) = {
      def aux_checkBoard(line: List[Char], wordList: List[(String,Boolean)], i: Int, j: Int): (Boolean, List[(String, Boolean)]) = {
        def checkWord(char: Char, wordList: List[(String,Boolean)], i: Int, j: Int): (Boolean, List[(String, Boolean)]) = wordList match {
          case Nil => (false, Nil)
          case x :: xs => val word = x._1.toList
            val a = checkWord(char, xs, i, j)
            if (a._1) { //if there is duplicate "break"
              (true, Nil)
            } else {
              if (word.head != char) { //does not evoke play if char isn't equal first letter of word
                (false || a._1, x :: a._2)
              } else {
                val play = _play(ogBoard, word, (i, j)::List[Coord2D](), Direction.getAllDirs)
                if (play._1) {
                  if (x._2) { //see if Word was already found, aka is duplicated
                    (true, Nil)
                  } else {
                    (false || a._1, (x._1, true) :: a._2)
                  }
                } else {
                  (false || a._1, x :: a._2)
                }
              }
            }
        }
        line match {
          case Nil => (false,wordList)
          case x :: xs => val a = checkWord(x, wordList, i, j)
            if(a._1){ //if there is duplicate "break"
              (true,Nil)
            }else{
              val b = aux_checkBoard(xs, a._2, i, j+1)
              (a._1||b._1, b._2)
            }
        }
      }
      board match {
        case Nil => (false,wordList)
        case x::xs => val a = aux_checkBoard(x, wordList, acc, 0)
          if(a._1){ //if there is duplicate "break"
            (true,Nil)
          }else{
            val b = _checkBoard(xs, a._2, acc+1)
            (a._1||b._1, b._2)
          }
      }
    }
    def wordFoundVerify(w: List[(String,Boolean)]): Boolean = {
      (w foldRight true)((e,acc) => e._2 && acc)
    }
    val a = _checkBoard(ogBoard, initializeWordList(wordList), 0) //a._1 return true if any duplicate found
    !a._1 && wordFoundVerify(a._2)
  }*/

  //T6
  def checkBoard(ogBoard: Board, words: List[String]): Boolean = {
    def _checkBoard(board: Board, word: List[Char], ogBoard: Board, i: Int, usedCoords: List[Coord2D]): (Boolean,Int) = {
      board match{
        case Nil => (false,0)
        case x::xs =>  if(word == Nil) {
          (true,1)
        }else{
          val a = aux_checkBoard(x,word, ogBoard, i, 0,usedCoords)
          val b = _checkBoard(xs, word,ogBoard, i+1,usedCoords)
          (a._1||b._1, a._2+b._2)
        }
      }
    }
    def aux_checkBoard(line: List[Char], word: List[Char], ogBoard: Board, i: Int, j: Int, usedCoords: List[Coord2D]): (Boolean,Int) = {
      line match{
        case Nil => (false,0)
        case x::xs => if(x == word.head) {
          val a = checkWord(word, ogBoard, (i,j),usedCoords)
          val b = aux_checkBoard(xs,word, ogBoard, i, j+1,usedCoords)
          if(a){
            (a||b._1, 1+b._2)
          }else{
            b
          }
        }else{
          aux_checkBoard(xs,word, ogBoard, i, j+1,usedCoords)
        }
      }
    }
    def checkWord(word: List[Char], ogBoard: Board, coord: Coord2D,  usedCoords: List[Coord2D]): (Boolean) = {
      def _checkWord(word: List[Char], ogBoard: Board, coord: Coord2D, dirs: List[Direction],  usedCoords: List[Coord2D]): (Boolean, Int) = {
        dirs match {
          case Nil => (false,0)
          case x::xs => if(word.length!=1){
            val a = _play(ogBoard, word, usedCoords:+coord, x::List[Direction]())
            if(a._1){
              val a = _checkWord(word, ogBoard, coord, xs, usedCoords)
              if(a._2==1){
                (false, 0)
              }else{
                (true, 1+a._2)
              }
            }else{
              _checkWord(word, ogBoard, coord, xs, usedCoords)
            }
          }else{
            (true,1)
          }
        }
      }

      val check = _checkWord(word, ogBoard, coord, Direction.getAllDirs, usedCoords)
      if(check._1 && check._2==1){
        val a = _checkBoard(ogBoard, word.tail, ogBoard, 0, usedCoords:+coord)
        if(a._1 /*&& a._2==1*/){
          true
        }else{
          false
        }
      }else{
        false
      }
    }

    words match {
      case x::Nil => val a = _checkBoard(ogBoard, x.toList, ogBoard, 0, List[Coord2D]())
        if(a._1 && a._2==1) {
          true
        }else{
          false
        }
      case x::xs => val a = _checkBoard(ogBoard, x.toList, ogBoard, 0, List[Coord2D]())
        if(a._1 && a._2==1) {
          checkBoard(ogBoard, xs)
        }else{
          false
        }
    }
  }


  //Initiazes the wordList with the words and all status to false (used in T6 and initializeBoard())
  def initializeWordList(words: List[String]): List[(String,Boolean)] = {
    (words foldRight List[(String,Boolean)]()) ((_,false)::_)
  }

  //Initializes the board at the beginning of the game
  def initializeBoard(rowWidth: => Int, columnHeight: => Int, fileName: String)(zigZag: ZigZag): ZigZag = {
    //Creates an empty board with the given dimensions, it exists because of rowWidth parameter reading (causing a loop of requesting the parameter otherwise)
    def createBoard(rowWidth: Int, columnHeight: Int): Board = {
      List.fill(columnHeight)(List.fill(rowWidth)(Empty))
    }
    val words = IO_Utils.loadWordsCoord(fileName)  //Loads a a tupple with a List of words and a List of a List of Point2D
    //val b = setBoardWithWords(createBoard(rowWidth, columnHeight),words._1,words._2)     //non optimized T3
    //val r = completeBoardRandomly(b, zigZag.rand, randomChar)                            //non optimized T4
    val r = setBoard(createBoard(rowWidth, columnHeight), zigZag.rand, randomChar, words._1, words._2)  //optimized T3 and T4
    val wordL = (initializeWordList(words._1),List())
    val newZigZag = new ZigZag(r._1, r._2, wordL, System.currentTimeMillis(), 0)
    if (!checkBoard(r._1, words._1)){
      IO_Utils.printBoard(newZigZag.board)
      IO_Utils.errorBoard()
      exit(SeedFile)(newZigZag)
    }else{
      newZigZag
    }
  }

  //Used by the TUI to select a word
  def selectWord(word: => String, coord2D: => Coord2D, dir: => Direction)(zigZag: ZigZag): ZigZag = {
    val w = word
    val pl = play(zigZag.board, w, coord2D,dir)
    if(isInWordList(w, zigZag.wordList._1) && pl._1) {
      IO_Utils.printResult(true)
      val wordL = (changeWordStatus(zigZag.wordList._1, w),zigZag.wordList._2:::pl._2)
      if (isGameCleared(wordL._1)) {
        val totalTime = (System.currentTimeMillis() - zigZag.time)/1000
        val score = calculateScore(totalTime, zigZag.failedPlays)
        IO_Utils.printBoard(zigZag.board, wordL._2)
        IO_Utils.printGameCleared(score, totalTime)
        reset()(zigZag)
      }else{
        new ZigZag(zigZag.board, zigZag.rand, wordL, zigZag.time, zigZag.failedPlays)
      }
    } else {
      IO_Utils.printResult(false)
      new ZigZag(zigZag.board, zigZag.rand, zigZag.wordList,zigZag.time, zigZag.failedPlays+1)
    }
  }

  //selectWord adapted to GUI
  def selectWord(word: String, coords: List[Coord2D])(zigZag: ZigZag): ZigZag = {
    val wordL = (changeWordStatus(zigZag.wordList._1, word), zigZag.wordList._2 ::: coords)
    new ZigZag(zigZag.board, zigZag.rand, wordL, zigZag.time, zigZag.failedPlays)
  }

  //Changes the word status to true (found), used by both selectWord methods
  def changeWordStatus(wordList: List[(String,Boolean)], word: String): List[(String,Boolean)] = {
    (wordList foldRight List[(String,Boolean)]())((e,acc) => if(e._1 == word) (e._1,true)::acc else e::acc)
  }

  //Verifies if word is in List
  def isInWordList(word: String, list: List[(String,Boolean)]): Boolean = {
    (list foldRight false)((e,acc) => if(e._1==word) true else acc)
  }

  //Calculates game score
  def calculateScore(time: Long, fPlays: Int): Long = {
    300 - time - 5*fPlays
  }

  //Verify if all the word were found, aka the game is cleared
  def isGameCleared(wordList: List[(String,Boolean)]): Boolean = {
    (wordList foldRight true)((e,acc) => e._2 && acc)
  }

  //Return the amount of words found
  def howManyWordsFound(wordList: List[(String,Boolean)]): Int = {
    (wordList foldRight 0)((e,acc) => if(e._2) acc+1 else acc)
  }

  //Resets the game
  def reset()(zigZag: ZigZag): ZigZag = {
    new ZigZag(List(List()), zigZag.rand, (List(),List()), 0, 0)
  }

  //Exits the game and save the current MyRandom seed to a file
  def exit(file: String)(zigZag: ZigZag): ZigZag = {
    IO_Utils.writeSeed(file, zigZag.rand.seed)
    sys.exit()
  }

  //Saves a game state to a save file
  def save()(zigZag: ZigZag): ZigZag = {
    val save = new ZigZag(zigZag.board, zigZag.rand, zigZag.wordList, System.currentTimeMillis()-zigZag.time, zigZag.failedPlays)
    IO_Utils.saveGame(save)
    zigZag
  }

  //Loads a game state from a save file
  def load(file: => String)(zigZag: ZigZag): ZigZag = {
    val f = file
    if(f==null) {
      zigZag
    }else{
      val load = IO_Utils.loadGame(file)
      new ZigZag(load.board, load.rand, load.wordList, System.currentTimeMillis()-load.time, load.failedPlays)
    }
  }

  //Play adapted to GUI with defined coordinates, receives a list of coordinates and verifies if the letters correspond with the chars on the coordinates
  //Decidido nao utilizar, pq a GUI é suposto usar as mesmas funções que a TUI
  @unused
  def playGUI(word: String, coords: List[Coord2D])(zigZag: ZigZag): Boolean = {
    @tailrec
    def _playGUI(board: Board, chars: List[Char], coords: List[Coord2D]): Boolean = chars match{
      case Nil => true
      case x::xs => if(x == getChar(board,coords.head)) _playGUI(board, xs, coords.tail) else false
    }
    if(isInWordList(word,zigZag.wordList._1)){
      _playGUI(zigZag.board, word.toList, coords)
    }else{
      false
    }
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
    val board3 = setBoardWithWords(board, List("DIOGO","AI"), List(List((1,0),(1,1),(1,2),(2,2),(1,3)),List((4,3),(3,3))))
    IO_Utils.printBoard(board3)

    //Teste T4
    println("\nT4:")
    val board4 = completeBoardRandomly(board3, r, randomChar)._1
    IO_Utils.printBoard(board4)

    //Teste T5
    println("\nT5:")
    val board5 = setBoardWithWords(board, List("DIOG","AI"), List(List((1,0),(1,1),(1,2),(2,2)),List((4,3),(3,4))))
    val board6 = completeBoardRandomly(board5, r, randomChar)._1
    IO_Utils.printBoard(board6)
    println()
    println(play(board6,"AI",(4,3), Direction.NorthEast))

    //Teste T6
    println("\nT6:")
    val board7 = List(List('D', 'O', 'O', 'G', 'O'), List('T', 'A', 'B', 'A', 'M'), List('E', 'V', 'O', 'E', 'B'), List('S', 'P', 'M', 'P', 'P'), List('P', 'T', 'A', 'I', 'Z'))
    IO_Utils.printBoard(board7)
    //println(_play(board7,"TEST".toList, (1,0)::List[Coord2D](),Direction.getAllDirs))
    println(checkBoard(board7,List("DIOGO","TEST")))
    println()
    val board8 = List(List('D', 'I', 'O', 'G', 'O'), List('T', 'A', 'B', 'A', 'M'), List('E', 'V', 'O', 'E', 'B'), List('S', 'P', 'M', 'P', 'P'), List('P', 'T', 'A', 'I', 'Z'))
    IO_Utils.printBoard(board8)
    //println(_play(board7,"TEST".toList, (1,0)::List[Coord2D](),Direction.getAllDirs))
    println(checkBoard(board8,List("DIOGO","TEST")))
    println()
    val board9 = List(List('D', 'I', 'O', 'G', 'O'), List('T', 'A', 'A', 'T', 'M'), List('E', 'S', 'T', 'E', 'B'), List('S', 'P', 'M', 'P', 'P'), List('P', 'T', 'A', 'I', 'Z'))
    IO_Utils.printBoard(board9)
    //println(_play(board7,"TEST".toList, (1,0)::List[Coord2D](),Direction.getAllDirs))
    println(checkBoard(board9,List("DIOGO","TEST")))
    println()
    println()
    val board10 = List(List('D', 'I', 'O', 'G', 'O'), List('T', 'A', 'G', 'D', 'M'), List('E', 'V', 'O', 'E', 'B'), List('S', 'L', 'M', 'P', 'P'), List('T', 'O', 'A', 'I', 'Z'))
    IO_Utils.printBoard(board10)
    //println(_play(board7,"TEST".toList, (1,0)::List[Coord2D](),Direction.getAllDirs))
    println(checkBoard(board10,List("DIOGO","TEST")))
  }

}
