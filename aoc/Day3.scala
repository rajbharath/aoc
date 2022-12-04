import scala.util.{Success, Failure, Try}

object Day3 extends App {
  
  type Compartment = String
  type Item = Char
  case class RuckSack(c1: Compartment, c2: Compartment)
  val itemValues = ('a' to 'z') ++ ('A' to 'Z')
  def getTypesInBothCompartments(r: RuckSack): Seq[Item] = for {
    i1 <- r.c1
    i2 <- r.c2
    if (i1 == i2)
  } yield {
    i1
  }

  def dedupe(c: Seq[Item]): Item = c.distinct.head

  def getPriority(item: Item): Int = itemValues.indexOf(item) + 1

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
  
  def getRuckSacks(lines: Iterator[String]): Iterator[RuckSack] = lines.map((line) => { 
    val compartments = line.splitAt(line.length()/2)
    RuckSack(compartments._1, compartments._2)
  })

  def sumOfItemPriorites(filename: String): Int = getRuckSacks(parseInput(filename)).toList
    .map(getTypesInBothCompartments)
    .map(dedupe)
    .map(getPriority)
    .sum

  println(sumOfItemPriorites("testday3.txt"))
  println(sumOfItemPriorites("inputday3.txt"))
}
