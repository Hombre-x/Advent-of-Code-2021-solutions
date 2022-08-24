package utils

import java.io.File
import cats.effect.IO
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}

extension [A](io: IO[A])
  def debug: IO[A] =
    for
      a <- io
      t = Thread.currentThread().getName
      _ = println(s"[$t] $a")
    yield a
  
  /**
   * This function evaluates the effect in another thread and joins the result.
   * After that, it pattern matches the result depending on the outcome of the Fiber.
   *
   * Similar behaviour of join in Cats Effect 2.
   * @param io an IO containing an effect.
   * @return if the outcome was neither errored nor cancelled, returns the result of the computation.
   */
  def retrieve: IO[A] =
    val fromOtherThread =
      for
        fib <- io.start
        result <- fib.join
      yield result
    
    fromOtherThread flatMap(
      outcome => outcome match
        case Succeeded(effect) => effect
        case Errored(error) => IO.raiseError(error)
        case Canceled() => IO.raiseError(new Exception("Fiber cancelled!"))
    )
    
