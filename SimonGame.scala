
import scala.util.Random
import scala.util.{Try, Success, Failure}
import scala.language.postfixOps

case class Button(color: String)

object SimonGame{
	def main(args: Array[String]){
		val redButton = Button("red")
		val blueButton =  Button("blue")
		val yellowButton = Button("yellow")
		val greenButton = Button("green")

		val buttons:List[Button] = List(redButton, blueButton, yellowButton, greenButton)

		def buttonsGen: Stream[Button] = {
			val rnd = new scala.util.Random
			buttons(rnd.nextInt(buttons length)) #:: buttonsGen
		}

		def showSequence(sequence: List[Button]){
			for(button <- sequence){
				println(button)
			}
		}

		def askButton(): Button = Button(scala.io.StdIn.readLine())

		val infiniteSequence = buttonsGen

		//Retourne le score
		def play(size: Int): Int = {
			val sequence = infiniteSequence.take(size).toList
			showSequence(sequence)
			completeSequence(sequence) match {
				case true => play(size + 1)
				case false => size - 1
			}
		}

		def completeSequence(sequence: List[Button]): Boolean = sequence match {
			case Nil => true
			case x::xs if askButton() == x => completeSequence(xs)
			case _ => false
		}

		val score = play(1) // Start the game
		println(s"Résultat : $score")
	}
}
