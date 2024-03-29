package ZigZagGame

import scala.annotation.tailrec
import scala.collection.SortedMap

object Main extends App {

  val rand = MyRandom(10)
  val game = ZigZag(rand)

  val options = SortedMap[Int, CommandLineOption](
    //1 -> new CommandLineOption("Iniciar tabuleiro", ZigZag.addEntryG(IO_Utils.prompt("Key"), IO_Utils.prompt("Value"))),
    //2 -> new CommandLineOption("Selecionar palavra", ZigZag.play( ....... )),
    //3 -> new CommandLineOption("Reiniciar", ZigZag.______( ....... )),
    //4 -> new CommandLineOption("Alterar cor do texto", ZigZag.______( ....... )),
    0 -> new CommandLineOption("Exit", _ => sys.exit)
  )

  mainLoop(game)

  @tailrec
  def mainLoop(game: ZigZag) {
    IO_Utils.optionPrompt(options) match {
      case Some(opt) => val newGame = opt.exec(game); mainLoop(newGame)
      case _ => println("Invalid option"); mainLoop(game)
    }
  }

}
