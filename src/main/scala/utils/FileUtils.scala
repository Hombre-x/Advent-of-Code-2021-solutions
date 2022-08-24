package utils

import cats.effect.{IO, Outcome, Resource}

import java.io.{File, PrintWriter}
import scala.io.Source

object FileUtils:
  
  // TODO: Implement a way of counting and returning the bytes written
  def open(path: String): IO[File] = IO(s"Opening file at: $path").debug >> IO(new File(path))
  
  
  def getLines(file: File): IO[Vector[String]] = 
    
    Resource
      .make(IO(Source.fromFile(file)))(bs => IO(bs.close()))
      .use(bs => IO("Getting lines ...").debug >> IO(bs.getLines().toVector))
  
  end getLines
  
  
  def getInput(path: String): IO[Vector[String]] =
    for
      file <- open(path)
      input <- getLines(file)
    yield input
  
  
  def write(content: String, outputFile: File): IO[Outcome[IO, Throwable, Unit]] =
    
    val writerResource = Resource.make(IO(new PrintWriter(outputFile)))(pw => IO(pw.close()).void)
    
    for
      fib <- IO("Writing lines ...").debug >>
        writerResource
          .use( pw => IO(pw.write(s"$content\n")) )
          .start
      
      outcome <- fib.join
    yield outcome
  
  end write
  
end FileUtils
