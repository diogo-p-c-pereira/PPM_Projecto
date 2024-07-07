package ZigZagGame

import javafx.animation.AnimationTimer
import javafx.fxml.FXML
import javafx.geometry.Pos
import javafx.scene.Scene
import javafx.scene.control.{Button, Label, SplitPane, ToggleButton}
import javafx.scene.layout.{GridPane, HBox, VBox}
import javafx.stage.{FileChooser, Modality, Stage}

import java.io.File
import scala.annotation.tailrec

class Controller {

  private val rand = MyRandom(IO_Utils.loadSeed(SeedFile))
  private var game = ZigZag(List(List()), rand, (List(),List()), 0, 0)

  @FXML
  private var grid: GridPane = _
  @FXML
  private var load: Button = _
  @FXML
  private var buttons: List[ToggleButton] = List()
  @FXML
  private var selectedButtons: List[ToggleButton] = List()
  @FXML
  private var correctButtons: List[ToggleButton] = List()
  @FXML
  private var wordsFound: Label = _
  @FXML
  private var timer: Label = _
  @FXML
  private var pane: SplitPane = _
  @FXML
  private var gameTimer: AnimationTimer = _


  def onInitializeClicked(): Unit = {
    if (!buttons.isEmpty) onResetClicked()
    grid.getScene.getWindow.setOnCloseRequest( _ => onExitClicked())
    game = game.initializeBoard(5, 5, WordsFile)
    buttons = List.fill(5*5)(new ToggleButton())
    setButtons(buttons, game.board)
    setHowManyWordsFound()
    startTimer()
  }

  private def setButtons(buttons: List[ToggleButton], board: List[List[Char]]): Unit = {
    @tailrec
    def _setButtons(list: List[ToggleButton], board: List[List[Char]], i: Int): Unit = {
      @tailrec
      def aux_setButtons(list: List[ToggleButton],chars: List[Char], j: Int): Unit = chars match{
        case Nil =>
        case x::xs => grid.add(list.head,j,i); setButtonLetter(list.head,x); aux_setButtons(list.tail,xs, j+1)
      }
      board match {
        case Nil =>
        case x :: xs => aux_setButtons(list.slice(0,5), x, 0); _setButtons(list.slice(5,list.length), xs, i+1)
      }
    }

    def setButtonLetter(button: ToggleButton, letter: Char): Unit = {
      button.setMaxSize(Double.MaxValue,Double.MaxValue)
      button.setText(letter.toString)
      button.setStyle("-fx-color: WHITE")
      button.setOnMouseClicked( _ => onLetterSelected(button))
    }

    _setButtons(buttons, board, 0)
  }



  private def onLetterSelected(button: ToggleButton): Unit = {
    def isValidLetter(button: ToggleButton, list: List[ToggleButton]): Boolean = list match {
      case Nil => true
      case _ => val last = list.last
        val coord1 =  (GridPane.getRowIndex(last).toInt, GridPane.getColumnIndex(last).toInt)
        val coord2 = (GridPane.getRowIndex(button).toInt, GridPane.getColumnIndex(button).toInt)
        Direction.isContiguous(coord1, coord2)
    }

    @tailrec
    def notInButtons(button: ToggleButton, list: List[ToggleButton]): Boolean = list match{
      case Nil => true
      case x::xs => if(x==button) false else notInButtons(button,xs)
    }

    if (notInButtons(button, correctButtons)) {
      if (notInButtons(button, selectedButtons) && isValidLetter(button, selectedButtons)) {
        if (button.isSelected) {
          if(!selectedButtons.isEmpty) selectedButtons.last.setStyle("-fx-color: WHITE")
          button.setStyle("-fx-color: LIGHTBLUE")
          selectedButtons = selectedButtons :+ button
          button.setSelected(true)
        }
      } else {
        if (button == selectedButtons.last) {
          selectedButtons = selectedButtons.filterNot(b => b==button)
          button.setStyle("-fx-color: WHITE")
          button.setSelected(false)
          if(!selectedButtons.isEmpty){
            selectedButtons.last.setStyle("-fx-color: LIGHTBLUE")
          }
        } else if(button.isSelected){
          button.setSelected(false)
        }else{
          button.setSelected(true)
        }
      }
    } else button.setSelected(true)
  }

  def onResetClicked(): Unit = {
    game = game.reset()
    gameTimer.stop()
    for(b <- buttons){
      grid.getChildren.remove(b)
    }
    timer.setText("Timer: 00:00")
    wordsFound.setText("Words found: ")
    buttons = List()
    selectedButtons = List()
    correctButtons = List()
  }

  def onExitClicked(): Unit = {
    game = game.exit(SeedFile)
  }

  def onEnterClicked(): Unit = {
    if(!buttons.isEmpty && selectedButtons.length!=1 && !selectedButtons.isEmpty){
      var word = ""
      for(b <- selectedButtons){
        word += b.getText
      }
      val coordHead = (GridPane.getRowIndex(selectedButtons.head).toInt, GridPane.getColumnIndex(selectedButtons.head).toInt)
      val coordSecond = (GridPane.getRowIndex(selectedButtons(1)).toInt, GridPane.getColumnIndex(selectedButtons(1)).toInt)
      val pl = game.play(word, coordHead, Direction.getDirection(coordHead, coordSecond))
      if(pl._1 && game.isInWordList(word)) {
        for(e <- selectedButtons){
          e.setStyle("-fx-color: LIGHTGREEN")
          correctButtons = e::correctButtons
        }
        selectedButtons = List()
        game = game.selectWord(word, pl._2)
        setHowManyWordsFound()
        if(game.isGameCleared){
          gameTimer.stop()
          setGameClearedMessage()
        }else{
          setMessageOK("Word Found!")
        }
      }else{
        setMessageOK("Incorrect Word")
        onClearClicked()
        game = game.incrementFailedPlays()
      }
    }
  }

  private def setHowManyWordsFound(): Unit = {
    wordsFound.setText("Words found: " + game.howManyWordsFound().toString + "/" + game.wordList._1.length)
  }

  def onClearClicked(): Unit = {
    for(e <- selectedButtons){
      e.setSelected(false)
      selectedButtons.last.setStyle("-fx-color: WHITE")
    }
    selectedButtons = List()
  }

  def loadGame(): Unit = {
    @tailrec
    def setButtonsState(coords: List[(Int, Int)]): Unit = {
      @tailrec
      def searchButton(coord: (Int, Int), b: List[ToggleButton]): Unit = b match {
        case Nil =>
        case b::bs => if(GridPane.getRowIndex(b) == coord._1 && GridPane.getColumnIndex(b) == coord._2){
          b.setStyle("-fx-color: LIGHTGREEN")
          correctButtons = b::correctButtons
          b.setSelected(true)
        }else{
          searchButton(coord, bs)
        }
      }

      coords match {
        case Nil =>
        case x::xs => searchButton(x, buttons); setButtonsState(xs)
      }
    }

    val fileChooser = new FileChooser()
    fileChooser.setInitialDirectory(new File("."))
    val extFilter = new FileChooser.ExtensionFilter("Save files (*.save)", "*.save")
    fileChooser.getExtensionFilters.add(extFilter)
    val file = fileChooser.showOpenDialog(load.getScene.getWindow)
    if(file != null){
      val newGame = game.load(file.getPath)
      if(newGame.board.size == 5 && newGame.board.head.size == 5){
        if (!buttons.isEmpty) onResetClicked()
        game = newGame
        buttons = List.fill(5*5)(new ToggleButton())
        setButtons(buttons, game.board)
        setButtonsState(game.wordList._2)
        startTimer()
      }else {
        setMessageOK("Invalid layout dimensions!")
      }
    }
  }

  def saveGame(): Unit = {
    game = game.save()
  }

  private def close(button: Button): Unit = {
    button.getScene.getWindow.hide()
  }

  private def setMessage(msg: String): (HBox, Stage)  = {
    val secondStage: Stage = new Stage()
    secondStage.initModality(Modality.APPLICATION_MODAL)
    val l = new Label(msg)
    l.setAlignment(Pos.CENTER)
    val h = new HBox(4)
    h.setAlignment(Pos.CENTER)
    h.getChildren.add(l)
    (h, secondStage)
  }

  private def setGameClearedMessage(): Unit = {
    val (h1, s) = setMessage("Game Cleared!!!")
    val b = (new Button(), new Button())
    b._1.setText("Reset")
    b._1.setOnMouseClicked( _ => {close(b._1); onResetClicked()})
    b._2.setText("Exit")
    b._2.setOnMouseClicked( _ => onExitClicked())
    s.setOnCloseRequest( _ => onResetClicked())

    val time = (System.currentTimeMillis() - game.time)/1000

    val l1 = new Label("Score: " + game.calculateScore(time) + "  ")
    val l2 = new Label("Time: " + String.format("%02d", time/60) + ":" + String.format("%02d", time%60))

    val h2 = new HBox(30)
    h2.setAlignment(Pos.CENTER)
    h2.getChildren.addAll(l1, l2)

    val h3 = new HBox(4)
    h3.setAlignment(Pos.CENTER)
    h3.getChildren.addAll(b._1, b._2)

    val v = new VBox(10)
    v.setAlignment(Pos.CENTER)
    v.getChildren.addAll(h1, h2, h3)

    s.setScene(new Scene(v, 200, 100))
    s.show()
  }


  private def setMessageOK(msg: String): Unit = {
    val (h1, s) = setMessage(msg)
    val b = new Button()
    b.setAlignment(Pos.BOTTOM_RIGHT)
    b.setText("OK")
    b.setOnMouseClicked( _ => close(b))

    val h2 = new HBox(4)
    h2.setAlignment(Pos.CENTER)
    h2.getChildren.addAll(b)

    val v = new VBox(10)
    v.setAlignment(Pos.CENTER)
    v.getChildren.addAll(h1, h2)

    s.setScene(new Scene(v, 200, 100))
    s.show()
  }

  def lockPaneDividers(): Unit = {
    val dividers = pane.getDividers
    dividers.get(0).setPosition(0.11094377510040163)
    dividers.get(1).setPosition(0.7986947791164659)
  }

  private def startTimer(): Unit = {
    def setTimerText(): Unit = {
      val time = (System.currentTimeMillis() - game.time)/1000
      timer.setText("Timer: " + String.format("%02d", time/60) + ":" + String.format("%02d", time%60))
    }

    gameTimer = new AnimationTimer() {
      override def handle(l: Long): Unit = {
        setTimerText()
      }
    }
    gameTimer.start()
  }

}
