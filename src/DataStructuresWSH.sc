object DataStructuresWSH {
  // ARRAY & STRING

  val xs = Array(2, 3, 5, 7, 11)                  //> xs  : Array[Int] = Array(2, 3, 5, 7, 11)
  val xs_string = "It codes as Value the Author's Name"
                                                  //> xs_string  : String = It codes as Value the Author's Name

  // change the sing of all xs elements
  // res0: Array[Int] = Array(-2, -3, -5, -7, -11)
  xs map (_ * -1)                                 //> res0: Array[Int] = Array(-2, -3, -5, -7, -11)

  // get String of all capital letters of xs_string
  // res1: String = IVAN
  xs_string filter (_.isUpper)                    //> res1: String = IVAN

  // RANGES

  // range: scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val range = 1 to 10                             //> range  : scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5, 6
                                                  //| , 7, 8, 9, 10)
  // range_a: scala.collection.immutable.Range = Range(1, 3, 5, 7, 9)
  val range_a = 1 to 10 by 2                      //> range_a  : scala.collection.immutable.Range = Range(1, 3, 5, 7, 9)

  // get range of all letters between 'a' and 'z' but without 'z'
  // range_b: scala.collection.immutable.NumericRange.Exclusive[Char] = NumericRange(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)
  val range_b = 'a' until 'z'                     //> range_b  : scala.collection.immutable.NumericRange.Exclusive[Char] = Numeric
                                                  //| Range(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x
                                                  //| , y)

  // the sum of x1*y1 + x2*y2 + ...
  def scalarProduct( x: Vector[Int], y: Vector[Int]) = ((x zip y) map { xy => xy._1 *  xy._2} ). sum
                                                  //> scalarProduct: (x: Vector[Int], y: Vector[Int])Int
  // res2: Int = 18
  scalarProduct(Vector(3, 3, 3), Vector(2, 2, 2)) //> res2: Int = 18

  // MAPS

  // numberNames: scala.collection.immutable.Map[Int,String] = Map(1 -> Two, 2 -> Three, 3 -> Four)
  val numberNames = Map (1 -> "Two", 2 -> "Three", 3 -> "Four")
                                                  //> numberNames  : scala.collection.immutable.Map[Int,String] = Map(1 -> Two, 2
                                                  //|  -> Three, 3 -> Four)
  // corrects numberNames in a new map where the key of every pair is shifted with +1
  // res3: scala.collection.immutable.Map[Int,String] = Map(2 -> Two, 3 -> Three, 4 -> Four)
  numberNames map ( xy => (xy._1 + 1, xy._2))     //> res3: scala.collection.immutable.Map[Int,String] = Map(2 -> Two, 3 -> Three
                                                  //| , 4 -> Four)

  // alternative syntax of the correcting map above - do the same but codded with the pattern matching
  // res4: scala.collection.immutable.Map[Int,String] = Map(2 -> Two, 3 -> Three, 4 -> Four)
  numberNames map { case (x, y) => (x + 1, y)}    //> res4: scala.collection.immutable.Map[Int,String] = Map(2 -> Two, 3 -> Three
                                                  //| , 4 -> Four)

  // the result for missing element : res5: Option[String] = None
  numberNames.get(-1)                             //> res5: Option[String] = None
  //the result for existing element: res6: Option[String] = Some(Two)
  numberNames.get(1)                              //> res6: Option[String] = Some(Two)
  // map called like a function - returns NoSuchElementException if the tlement does not exist (uncomment the next line to test)
  // numberNames(-1)

  //checks if n is prime number, if it can't be divided by no one of the elements between 2 and (n-1)
  def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)
                                                  //> isPrime: (n: Int)Boolean

  // res7: Boolean = true
  isPrime(7)                                      //> res7: Boolean = true
  //res8: Boolean = false
  isPrime(8)                                      //> res8: Boolean = false

  // STREAMS

  // represents stream in the range between two integers
  def streamRange(from: Int, to: Int) : Stream[Int] = {
    if (from > to) Stream.empty
    else Stream.cons(from, streamRange(from + 1, to))
  }                                               //> streamRange: (from: Int, to: Int)Stream[Int]

  // represents list in the range between two integers
  def listRange(from: Int, to: Int) : List[Int] = {
    if (from > to) List.empty
    else from :: listRange(from + 1, to)
  }                                               //> listRange: (from: Int, to: Int)List[Int]

  /*
   We see that only the first element '1' is calculated,
   the rest is not known, marked with "?", until it's needed.
   */
  // res9: Stream[Int] = Stream(1, ?)
  streamRange(1, 10)                              //> res9: Stream[Int] = Stream(1, ?)

  /*
   We see calculation the the full list.
   */
  // res10: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  listRange(1, 10)                                //> res10: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  /*
   Calculates for any of the numbers between 5000 and 50000
   if it's prime after that filters these ones with 'true'.
   At the end retuns the fist of the filtered elements.
   The number of isPrime calls is 45001.
   */
  // res11: Int = 5003
  ((5000 to 50000) filter (isPrime(_))) (0)       //> res11: Int = 5003

  /*
   Filters the elements from 5000 one by one until reaches 'Stream(5003, ?)'
   element. And because 5003 is prime number - it won't calculate futher until it's needed.
   */
  // res12: scala.collection.immutable.Stream[Int] = Stream(5003, ?)
  ((5000 to 50000).toStream filter (isPrime(_)))  //> res12: scala.collection.immutable.Stream[Int] = Stream(5003, ?)

  /*
   Gets the first element of the function call above.
   The number of isPrime calls is only 4 (compared to 45001 in the variant
   where List are used instead of Stream).
   */
  // res13: Int = 5003
  ((5000 to 50000).toStream filter (isPrime(_))) (0)
                                                  //> res13: Int = 5003

  // define infinite stream of numbers
  def from(n:Int): Stream[Int] = Stream.cons(n, from(n + 1))
                                                  //> from: (n: Int)Stream[Int]

  /*
   Only one line implementation of "Sieve of Eratosthenes" algorithm.
   https://bg.wikipedia.org/wiki/%D0%A0%D0%B5%D1%88%D0%B5%D1%82%D0%BE_%D0%BD%D0%B0_%D0%95%D1%80%D0%B0%D1%82%D0%BE%D1%81%D1%82%D0%B5%D0%BD
   */
  def eratosten(s: Stream[Int]): Stream[Int] =
    Stream.cons(s.head, eratosten(s.tail filter (_ % s.head != 0)))
                                                  //> eratosten: (s: Stream[Int])Stream[Int]

  // Shows first 100 prime numbers using "steve of Eratosten" algorithm.
  // res14: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541)
  eratosten(from(2)).take(100).toList             //> res14: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
                                                  //|  47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 
                                                  //| 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 
                                                  //| 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 
                                                  //| 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 
                                                  //| 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 
                                                  //| 479, 487, 491, 499, 503, 509, 521, 523, 541)
}