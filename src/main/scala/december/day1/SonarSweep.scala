package december.day1

import scala.annotation.tailrec

object SonarSweep:
  
  object Part1:
  
    def measureDepth(data: Vector[Int]): Int =
  
      val zippedData: Vector[(Int, Int)] = 0 +: data zip data.init

      zippedData.foldLeft(0)( (total, measure) =>
          if measure._1 < measure._2 then total + 1
          else total
      )
      
    end measureDepth
      
  end Part1
  
  
  object Part2:
    
    import Part1._
    
    def groupAndSum(data: Vector[Int]): Vector[Int] =
      
      @tailrec
      def tailGroup(data: Vector[Int], accum: Vector[Int] = Vector.empty): Vector[Int] =
        
        data match
          
          case first +: second +: third +: Vector() =>
            accum :+ (first + second + third)
        
          case first +: second +: third +: _ =>
            tailGroup(data.tail, accum :+ first + second + third)
            
          case _ => accum
          
      tailGroup(data)
      
    end groupAndSum
    
  end Part2
  
  
  export Part1.measureDepth
  export Part2.groupAndSum
  
end SonarSweep


  

