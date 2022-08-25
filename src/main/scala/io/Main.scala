package io

// Java imports
import december.day3.BinaryDiagnostic.Part1.gamma

import java.io.{File, PrintWriter}

// Scala imports
import scala.io.Source
import scala.util.Try

// Cats Imports
import cats.Show
import cats.effect.{IO, IOApp, Outcome, Ref, Resource}
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}

// My Code Imports
import utils.FileUtils._
import utils.{debug, retrieve}
import utils.GeneralUtils.retrieveSolutions
import utils.CustomInstances.given

object Programs:
  
  import december._
  // Programs:
  val day1answers: IO[(Int, Int)] =
  
    import day1.SonarSweep._
    
    for
      input <- getInput("src/main/Resources/Inputs/day1input.data")
      outFile <- open("src/main/Resources/Results/day1results.data")
      measurements = input.map(str => str.toInt)
      part1 <- IO.pure(measureDepth(measurements))
      part2 <- IO.pure( (measureDepth compose groupAndSum)(measurements) )
      _ <- write(s"$part1, $part2", outFile)
    yield (part1, part2)
  
  end day1answers
  
  val day2answers: IO[(Long, Long)] =
    
    import day2.Dive._
    
    for
      input <- getInput("src/main/Resources/Inputs/day2input.data")
      outfile <- open("src/main/Resources/Results/day2results.data")
      measurements =
        input.map(
          str =>
            val content = str.split(' ')
            (content(0), content(1).toInt)
        )
      part1 <- IO.pure(totalMovement(measurements))
      part2 <- IO.pure(aimMovement(measurements))
      _ <- write(s"$part1, $part2", outfile)
    yield (part1, part2)
    
  end day2answers
  
  val day3answers =
  
    import day3.BinaryDiagnostic
    
    retrieveSolutions("src/main/Resources/Inputs/day3input.data",
      "src/main/Resources/Results/day3results.data"){ (vec: Vector[String]) =>
      vec.map(_.toList.map(_.asDigit.toByte)).toList }(bitMap => gamma(bitMap), _ => "()")
    
end Programs

  
object Main extends IOApp.Simple:
  
  import Programs._
  override def run: IO[Unit] =
    for
      day3 <- day3answers
      _ <- IO.println(day3)
    yield ()
  
end Main
