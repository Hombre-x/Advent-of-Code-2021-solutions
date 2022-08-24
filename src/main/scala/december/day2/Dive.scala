package december.day2

import scala.annotation.tailrec

object Dive:
  
  object Part1:
    
    def totalMovement(data: Vector[(String, Int)]): Long =
  
      @tailrec
      def tailMovement(curr: Vector[(String, Int)], accum: (Int, Int)): (Int, Int) =
        
        if curr.isEmpty then accum
        else
          val (direction, value) = curr.head
          
          direction match
            case "forward" => tailMovement(curr.tail, (accum._1 + value, accum._2))
            case "up"      => tailMovement(curr.tail, (accum._1, accum._2 - value))
            case "down"    => tailMovement(curr.tail, (accum._1, accum._2 + value))
            case _         => tailMovement(curr.tail, (accum._1, accum._2))
            
      val (horizontal, depth) = tailMovement(data, (0, 0))
      
      horizontal * depth
      
    end totalMovement
    
  end Part1
  
  
  object Part2:

    def aimMovement(data: Vector[(String, Int)]): Long =
      
      @tailrec
      def tailMovement(curr: Vector[(String, Int)], aim: Int, accum: (Int, Int)): (Int, Int) =
        
        // (horizontal position, depth)
        
        if curr.isEmpty then accum
        else
          val (direction, value) = curr.head
          
          direction match
            case "forward" => tailMovement(curr.tail, aim, (accum._1 + value, accum._2 + aim * value))
            case "up"      => tailMovement(curr.tail, aim - value, (accum._1, accum._2))
            case "down"    => tailMovement(curr.tail, aim + value, (accum._1, accum._2))
            case _         => tailMovement(curr.tail, aim, (accum._1, accum._2))
      
      val (horizontal, depth) = tailMovement(data, 0, (0, 0))
      
      horizontal * depth
    
    end aimMovement
  
  end Part2
  
  
  export Part1.totalMovement
  export Part2.aimMovement
  
end Dive
