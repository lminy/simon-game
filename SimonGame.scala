import scala.util.Random
import scala.util.{Try, Success, Failure}
import scala.language.postfixOps

class Button

case class RedButton(sound: String) extends Button
case class BlueButton(sound: String) extends Button
case class YellowButton(sound: String) extends Button
case class GreenButton(sound: String) extends Button

object SimonGame{
	def main(args: Array[String]){
		val redButton = RedButton("red")
		val blueButton =  BlueButton("blue")
		val yellowButton = YellowButton("yellow")
		val greenButton = GreenButton("green")

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

		def askButton(): Option[Button] = {
			scala.io.StdIn.readLine() match{
				case "red" => Some(redButton)
				case "blue" => Some(blueButton)
				case "yellow" => Some(yellowButton)
				case "green" => Some(greenButton)
				case _ => None
			}
		}

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
			case x::xs if askButton().getOrElse(greenButton) == x => completeSequence(xs)
			case _ => false
		}

		val score = play(1) // Start the game
		println(s"Résultat : $score")


	}
}
