import scala.util.{Success, Failure, Try}

object Day4 extends App {
  case class SectionRange(start: Int, end: Int)
  case class SectionRangePair(p1: SectionRange, p2: SectionRange)

  def isFullyOverlapping(r1: SectionRange, r2: SectionRange): Boolean = (r1.start >= r2.start && r1.end <= r2.end)

  def hasFullOverlappingPair(pair: SectionRangePair): Boolean = 
    isFullyOverlapping(pair.p1, pair.p2) || isFullyOverlapping(pair.p2, pair.p1)

  def hasNoOverlapAtAll(r1: SectionRange, r2: SectionRange): Boolean = 
    (r1.start < r2.start && r1.end < r2.start)

  def hasNoOverlappingPair(pair: SectionRangePair): Boolean = 
    hasNoOverlapAtAll(pair.p1, pair.p2) || hasNoOverlapAtAll(pair.p2, pair.p1)

  def createSectionRange(sectionStr: String) = {
     val sections = sectionStr.split("-")
     SectionRange(sections(0).toInt, sections(1).toInt)
  }

  def createSectionRangePair(line: String) = {
     val sections = line.split(",")
     SectionRangePair(
      createSectionRange(sections(0)),
      createSectionRange(sections(1))
    )
  }

  def getSectionRanges(lines: Iterator[String]) = lines.map(createSectionRangePair)
  
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
  
  def numberOfFullyOverlappingSections(filename: String) = getSectionRanges(parseInput(filename))
    .map(hasFullOverlappingPair)
    .count(x => x == true)
  
  def numberOfAnyOverlappingSections(filename: String) = getSectionRanges(parseInput(filename))
    .map(hasNoOverlappingPair)
    .map(x => !x)
    .count(x => x == true)

// Part1
  println(numberOfFullyOverlappingSections("testday4.txt"))
  println(numberOfFullyOverlappingSections("inputday4.txt"))

// Part2
  println(numberOfAnyOverlappingSections("testday4.txt"))
  println(numberOfAnyOverlappingSections("inputday4.txt"))

}