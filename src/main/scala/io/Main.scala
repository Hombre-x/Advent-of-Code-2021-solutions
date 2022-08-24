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
import december._
import utils.FileUtils._
import utils.{debug, retrieve}


def retrieveSolutions[I, A, B](inputPath: String, outputPath: String)
                        (inputTransformer: Vector[String] => I)
                        (compute1: I => A, compute2: I => B): IO[String] =
  
  for
    inputFile <- open(inputPath)
    outputFile <- open(outputPath)
    input <- getLines(inputFile)
    measurements = inputTransformer(input)
    sol1 <- IO("Computing first solution") >> IO(compute1(measurements)).retrieve
    sol2 <- IO("Computing first solution") >> IO(compute2(measurements)).retrieve
    content = s"Part 1: $sol1. Part 2: $sol2"
    _ <- write(content, outputFile)
  yield content
  
end retrieveSolutions


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


val day2answers =
  
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

import day3.BinaryDiagnostic

val day3answers =
  retrieveSolutions("src/main/Resources/Inputs/day3input.data",
    "src/main/Resources/Results/day3results.data"){ (vec: Vector[String]) =>
    vec.map(_.toList.map(_.asDigit.toByte)).toList }(bitMap => gamma(bitMap), _ => ())
  
object Main extends IOApp.Simple:
  
  given tupleShow[A, B]: Show[(A, B)] with
    override def show(tup: (A, B)): String =
      s"(${tup._1}, ${tup._2})"
  
  override def run: IO[Unit] =
    for
      // sol <- day3answers
      result <- IO("From other thread!").debug >> IO(new RuntimeException("AAAAAa")).debug.retrieve
      _ <- IO(result).debug
    yield ()
  
end Main
