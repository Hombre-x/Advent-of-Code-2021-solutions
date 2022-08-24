package december.day3

object BinaryDiagnostic:

  object Part1:
    
    /**
     * O(n) x O(m), but m is almost always a constant, so it could be threated as O(n).
     * @param xs should contain at least one element.
     */
    def gamma(xs: List[List[Byte]]): List[Byte] =
      xs.transpose.map(_.groupBy(identity).maxBy { case (key, list) => list.size }._1)
    
  end Part1
  
  export Part1.gamma
