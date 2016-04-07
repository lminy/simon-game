// scalac -classpath .;*.jar SimonGame.scala
// scala -classpath .;*.jar SimonGame

import scala.util.Random
import scala.util.{Try, Success, Failure}
import scala.language.postfixOps
import java.io._

// To play MP3 files
import javazoom.jl.player.Player

case class Button(color: String)

// Interaction - VIEW
object ButtonsManager{

	val sounds = Map("red"    -> "cat.mp3",
					 "blue"   -> "dog.mp3",
					 "yellow" -> "cow.mp3",
					 "green"  -> "pig.mp3"
	)

	def askButton(): Button = {
		val color = scala.io.StdIn.readLine()
		if(sounds contains color) playSound(sounds(color))
		Button(color)
	}

	def showSequence(sequence: List[Button]){
		for(Button(color) <- sequence){
			println(color)
			playSound(sounds(color))
		}
	}

	def playSound(path: String) = {
		val file = new File(path)
		val in = new FileInputStream(file.getAbsoluteFile)
		val input = new BufferedInputStream(in)
		val player = new Player(input) // jl1.0.jar (JLayer Mp3 library) http://www.javazoom.net/javalayer/sources.html
		player.play()
		/*
		while(true){
			player.play(1)
		}*/
	}
}

class SimonGame{

	val redButton = Button("red")
	val blueButton =  Button("blue")
	val yellowButton = Button("yellow")
	val greenButton = Button("green")

	val buttons:List[Button] = List(redButton, blueButton, yellowButton, greenButton)

	val infiniteSequence = buttonsGen

	def start():Int = play(1) // Start the game

	//Retourne le score
	def play(size: Int): Int = {
		val sequence = infiniteSequence.take(size).toList
		ButtonsManager.showSequence(sequence)
		completeSequence(sequence) match {
			case true => play(size + 1)
			case false => size - 1
		}
	}

	def completeSequence(sequence: List[Button]): Boolean = sequence match {
		case Nil => true
		case x::xs if ButtonsManager.askButton() == x => completeSequence(xs)
		case _ => false
	}

	def buttonsGen: Stream[Button] = {
		val rnd = new scala.util.Random
		buttons(rnd.nextInt(buttons length)) #:: buttonsGen
	}
}

object SimonGame{
	def main(args: Array[String]){
		val game = new SimonGame()
		val score = game.start()
		println(s"Result : $score")
	}
}
