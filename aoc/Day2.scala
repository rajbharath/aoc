import scala.util.{Success, Failure, Try}

object Play extends Enumeration {
  type Play = Value
  val Rock, Scissors, Paper = Value
}

object Result extends Enumeration {
  type Result = Value
  val Won, Draw, Lose = Value
}

import Play._
import Result._

case class Turn(val p1: Play, val p2: Play)
case class Outcome(val p1: String, val p2: String)

object Day2 extends App {
  val weights = Map(
    Play.Rock -> 1, 
    Play.Paper -> 2,
    Play.Scissors -> 3, 
  )

  val scores = Map(
    Result.Won -> 6,
    Result.Draw -> 3,
    Result.Lose -> 0
  )
  
  def getScore(res: Result): Int = scores.getOrElse(res, 0)

  def getWeight(play: Play): Int = weights.getOrElse(play, 0)

  def getResult(t: Turn): (Result, Result) = t match {
    case Turn(p1, p2) if p1 == p2 => (Result.Draw, Result.Draw)
    case Turn(Play.Rock, Play.Scissors) | Turn(Play.Scissors, Play.Paper) | Turn(Play.Paper, Play.Rock) => (Result.Won, Result.Lose)
    case _ => (Result.Lose, Result.Won)
  }

  def getPlay(char: Char) = char match {
    case ('A' | 'X') => Play.Rock
    case ('B' | 'Y') => Play.Paper
    case ('C' | 'Z') => Play.Scissors
  }

  def getTurn(line: String)  = {
    line.charAt(0)
  }
  
  def parseInput(filename: String): List[Turn] = 
    Try {
      val source = scala.io.Source.fromFile(filename)
      source.getLines().map((line) => {
        Turn(getPlay(line.charAt(0)), getPlay(line.charAt(2)))
      }).toList
    } match {
      case Success(turns) => turns
      case Failure(e) => {
        println(e)
        List.empty
      }
    }

  def getScoresPart1(turns: List[Turn]): Int = {
    val score =
          turns
          .map(getResult)
          .map((r) => (getScore(r._1), getScore(r._2)))
          .foldLeft((0,0)){ case ((accScore1, accScore2), (score1, score2)) => (accScore1 + score1, accScore2 + score2) }
      
    val weight =
        turns
        .map((turn) => (getWeight(turn.p1), getWeight(turn.p2)))
        .foldLeft((0,0)){ case ((accWeight1, accWeight2), (weight1, weight2)) => (accWeight1 + weight1, accWeight2 + weight2) }

    score._2 + weight._2
  }
 
  print(getScoresPart1(parseInput("testday2.txt")))
  print(getScoresPart1(parseInput("inputday2.txt")))
  
}

