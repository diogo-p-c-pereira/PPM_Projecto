package ZigZagGame

import ZigZagGame.ZigZag.Direction

import scala.annotation.tailrec
import scala.collection.SortedMap

object Main extends App {

  val SeedFile = "seed.txt"
  val WordsFile = "words.txt"

  val seed = IO_Utils.loadSeed(SeedFile)
  val rand = MyRandom(seed)
  val game = ZigZag(List(List()), rand)

  val options = SortedMap[Int, CommandLineOption](
    1 -> new CommandLineOption("Iniciar tabuleiro",
      ZigZag.initializeBoard(IO_Utils.getUserInputInt("Nº de linhas").get, IO_Utils.getUserInputInt("Nº de Colunas").get, WordsFile)),
    2 -> new CommandLineOption("Selecionar palavra",
        ZigZag.selectWord(IO_Utils.prompt("Palavra"),(IO_Utils.getUserInputInt("Linha").get,IO_Utils.getUserInputInt("Coluna").get), IO_Utils.prompt("Direção").toUpperCase)),
    3 -> new CommandLineOption("Reiniciar", ZigZag.reset()),
    4 -> new CommandLineOption("Alterar cor do texto", ZigZag.changeColor(IO_Utils.prompt("Cor"))),
    0 -> new CommandLineOption("Exit", ZigZag.exit(SeedFile))
  )

  mainLoop(game)

  @tailrec
  def mainLoop(zigZag: ZigZag) {
    IO_Utils.printBoard(zigZag.board)
    IO_Utils.optionPrompt(options) match {
      case Some(opt) => val newZigZag = opt.exec(zigZag); mainLoop(newZigZag)
      case _ => println("Invalid option"); mainLoop(zigZag)
    }
  }

}
