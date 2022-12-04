import scala.util.{Success, Failure, Try}


object Day1 extends App {
  def totalCaloriesFromTop(calories: List[Int], n: Int) = calories.sorted.takeRight(n).sum

  def parseInput(filename: String): List[List[Int]] = 
    Try {
      val source = scala.io.Source.fromFile(filename)
      source.getLines().foldRight(List[List[Int]](List()))((line, result) => {
        if(line.isBlank()) {
          result :+ List[Int]()
        } else {
          result.dropRight(1) :+ (result.last :+ line.toInt)
        }
      })
    } match {
      case Success(lines) => lines
      case Failure(e) => {
        println(e)
        List.empty
      }
    }
    
  val testCalories = parseInput("testday1.txt").map((l: List[Int]) => l.sum)
  println(totalCaloriesFromTop(testCalories, 1))
  println(totalCaloriesFromTop(testCalories, 3))

  val calories = parseInput("inputday1.txt").map((l: List[Int]) => l.sum)
  println(totalCaloriesFromTop(calories, 1))
  println(totalCaloriesFromTop(calories, 3))
}