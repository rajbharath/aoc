import scala.util.{Success, Failure, Try}
import java.util.Stack

object Day5 extends App {

  type CrateStack = Stack[String]
  case class Rule(n: Int, source: Int, destination: Int)


  // Factory Methods
  def toRule(r: String) = {
     val parseResult = r.replace("move", "").replace("from", "").replace("to", "").split(" ").map(a => a.trim()).filter(!_.isEmpty)
     Rule(parseResult(0).toInt, parseResult(1).toInt, parseResult(2).toInt)
  }

  def createCrateStacks(lines: Iterator[String], numOfStack: Int): (List[CrateStack], List[Rule]) = {
    val stacks = (for (i <- 1 to numOfStack) yield new CrateStack()).toList
    lines.foldRight((stacks, List[Rule]()))((line: String, result) => {
      line match {
        case(a: String) if a.startsWith("move") => (result._1, toRule(a) :: result._2 )
        case(a: String) if a.length > 0 => {
          val crates = a.grouped(4).toList
          for (i <- 0 to (numOfStack-1)) yield {
            val crate = crates(i).trim()
            if (!crate.isBlank()) {
              stacks(i).push(crate)
            }
          }
          (stacks, result._2)
        }
        case _  => result
      }
    })
  }

  def parseInput(filename: String): Iterator[String] = 
    Try {
      val source = scala.io.Source.fromFile(filename)
      source.getLines()
    } match {
      case Success(lines) => lines
      case Failure(e) => {
        println(e)
        Iterator[String]()
      }
    }

  // Behaviours
  // Part2
  def moveTogether(rule: Rule, cratesStacks: List[CrateStack])= {
    val tempStack = new CrateStack()
    for(i <- 1 to rule.n) yield {
      tempStack.push(cratesStacks(rule.source - 1).pop())
    }
    for(i <- 1 to rule.n) yield {
      cratesStacks(rule.destination - 1).push(tempStack.pop())
    }
  }

  // Part1
  def move(rule: Rule, cratesStacks: List[CrateStack])= {
    for(i <- 1 to rule.n) yield {
      cratesStacks(rule.destination - 1).push(cratesStacks(rule.source - 1).pop())
    }
  }

  def getStacksWithRules(filename: String) = createCrateStacks(parseInput(filename), ((parseInput(filename).toList.head.length + 1)/4))

  def applyRules(stacks: List[CrateStack], rules: List[Rule], moveBehaviour: (Rule, List[CrateStack]) => Unit) =  for(rule <- rules) {
      moveBehaviour(rule, stacks.toList)
  }

  def getTopCrateOfAllStacks(stacks: List[CrateStack], rules: List[Rule]): String = {
    // Change here for different moveBehaviour
    applyRules(stacks, rules, moveTogether)
    stacks.map(s => s.pop()).mkString.replace("[", "").replace("]", "")
  }


  println((getTopCrateOfAllStacks _).tupled(getStacksWithRules("testday5.txt")))
  println((getTopCrateOfAllStacks _).tupled(getStacksWithRules("inputday5.txt")))

}