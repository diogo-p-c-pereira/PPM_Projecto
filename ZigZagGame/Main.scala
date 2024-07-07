package ZigZagGame

import scala.annotation.tailrec
import scala.collection.SortedMap

object Main extends App {

  val rand = MyRandom(IO_Utils.loadSeed(SeedFile))
  val game = ZigZag(List(List()), rand, (List(),List()), 0, 0)

  val options = SortedMap[Int, CommandLineOption](
    1 -> CommandLineOption("Iniciar tabuleiro",
      ZigZag.initializeBoard(IO_Utils.getUserInputInt("Nº de linhas").get, IO_Utils.getUserInputInt("Nº de Colunas").get, WordsFile)),
    2 -> CommandLineOption("Selecionar palavra",
      ZigZag.selectWord(IO_Utils.prompt("Palavra"), (IO_Utils.getUserInputInt("Linha").get, IO_Utils.getUserInputInt("Coluna").get), IO_Utils.dirPrompt())),
    3 -> CommandLineOption("Reiniciar", ZigZag.reset()),
    4 -> CommandLineOption("Guardar", ZigZag.save()),
    5 -> CommandLineOption("Load", ZigZag.load(IO_Utils.filePrompt())),
    0 -> CommandLineOption("Exit", ZigZag.exit(SeedFile))
  )

  mainLoop(game)

  @tailrec
  def mainLoop(zigZag: ZigZag) {
    if (zigZag.wordList._1.nonEmpty) IO_Utils.printWordsFound(zigZag.howManyWordsFound(), zigZag.wordList._1.length)
    IO_Utils.printBoard(zigZag.board, zigZag.wordList._2)
    IO_Utils.optionPrompt(options) match {
      case Some(opt) => val newZigZag = opt.exec(zigZag); mainLoop(newZigZag)
      case _ => IO_Utils.printInvalidOption(); mainLoop(zigZag)
    }
  }
}
