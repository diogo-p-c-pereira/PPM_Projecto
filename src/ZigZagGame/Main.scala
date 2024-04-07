package ZigZagGame

import scala.annotation.tailrec
import scala.collection.SortedMap

object Main extends App {

  //TODO Carregar de um ficheiro binário o MyRandom e gravar no final de cada execução
  val rand = MyRandom(10)
  val game = ZigZag(List(List()), rand)

  val options = SortedMap[Int, CommandLineOption](
    1 -> new CommandLineOption("Iniciar tabuleiro", ZigZag.initializeBoard(5, 5)), //TODO inserir dados pela consola, esta a dar bug
    //2 -> new CommandLineOption("Selecionar palavra", ZigZag.play( ....... )),
    3 -> new CommandLineOption("Reiniciar", ZigZag.reset()),
    //4 -> new CommandLineOption("Alterar cor do texto", _ =>  ),
    0 -> new CommandLineOption("Exit", _ => sys.exit)
  )

  /*val colorOptions = SortedMap[Int, CommandLineOption](
    1 -> new CommandLineOption("Vermelho", _ => print(Console.RED)),
    2 -> new CommandLineOption("Verde", _ => print(Console.GREEN)),
    3 -> new CommandLineOption("Amarelo", _ => print(Console.YELLOW)),
    4 -> new CommandLineOption("Azul", _ => print(Console.BLUE)),
    0 -> new CommandLineOption("Voltar", _ => mainLoop(game))
  )*/

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
