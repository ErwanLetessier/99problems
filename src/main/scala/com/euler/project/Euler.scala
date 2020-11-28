package com.euler.project

import com.example._99problems.ArithmeticProblems.{PrimeFactorContext, isPrime}
import com.example._99problems.Util.time

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object Euler {

  def fibonacci: Stream[BigInt] = {
    def nextFibonacci(n2: BigInt, n1: BigInt): Stream[BigInt] = {
      val n: BigInt = n2 + n1
      n #:: nextFibonacci(n1, n)
    }
    BigInt(0) #:: BigInt(1) #:: nextFibonacci(0,1)
  }

  def evenFibonacciSum(until: BigInt): BigInt = {
    fibonacci
      .filter(_ % 2 == 0)
      .takeWhile(_ < until)
      .sum
  }


  def primesUntil(n: Long): Stream[Long] = Stream.from(2).filter(isPrime).takeWhile(_< n).map(_.toLong)

  def primeFactors(n: Long): List[Long] = {
    val primes = primesUntil(n.toLong).toList
    @tailrec def findPrimeFactors(n: Long, acc: List[Long] = Nil): List[Long] = {
      if (n < 2) acc
      else {
        primes.find(n % _ == 0) match {
          case None => acc
          case Some(p) => findPrimeFactors(n/p, acc :+ p)
        }
      }
    }
    findPrimeFactors(n)
  }

  def largestPrimeFactor(n: Long): Long = {
    primeFactors(n).last
  }

  def isPalindrome(n: Int): Boolean = {
    n.toString == n.toString.reverse
  }

  def largestPalindromeProduct(min: Int, max: Int): (Int, Int) = {
    (for {
      i <- min to max
      j <- i to max
    } yield (i, j))
      .collect { case terms@(i, j) if isPalindrome(i * j) => (i * j) -> terms }
      .maxBy { case (k, _) => k } match {
      case (_, terms) => terms
    }
  }

  def smallestMultiple(a: Int, b: Int): Int = {
    val pfc = PrimeFactorContext(b)
    (a to b)
      .filter(_ > 1)
      .flatMap(pfc.primeFactorsCardinality)
      .groupBy{ case (prime, _) => prime }
      .map{ case (prime, l) => (prime, l.map{case(_, c) => c}.max) }
      .map { case (prime, multiplicity) => math.pow(prime, multiplicity).toInt }
      .product
  }

  def sum(seq: Seq[Int]): Int = {
    seq.sum
  }

  def square(i: Int): Int = {
    math.pow(i, 2).toInt
  }

  def sumSquareDifference(min: Int, max: Int): Int = {
    val sumOfSquares = (min to max).map(square).sum
    val squareOfSum = (sum _ andThen square) (min to max)
    squareOfSum - sumOfSquares
  }

  def nthPrime(n: Int): Int = {
    Stream.from(2)
      .filter(isPrime)
      .zip(Stream.from(1))
      .collect{case(p, i) if i >= n => p}
      .head
  }

  def largestProductInSeries(series: String, digits: Int): (String, Long) = {
    Stream.from(0)
      .takeWhile(_ < series.length - digits)
      .map(startIndex => series.substring(startIndex, startIndex + digits))
      .filterNot(_.contains("0"))
      .map(s => (s, s.toCharArray.map(_.toString).map(_.toLong).product))
      .maxBy{case(_, p) => p}
  }

  def intHypothenus(a: Int, b: Int): Option[Int] = {
    Option(math.sqrt(square(a) + square(b))).collect{ case c if c.isValidInt => c.toInt}
  }

  implicit class RichInt(i: Int) {
    def **(p: Int): Int = { math.pow(i, p).toInt}
  }

  def pythagoreanTriplet(targetSum: Int): Option[Vector[Int]] = {
    (
      for {
        aa <- Stream.from(1).takeWhile(_< targetSum)
        arr <- Stream.from(aa)
          .map(b => (aa, b, targetSum - aa - b))
          .dropWhile{ case (a, b, c) => a + b < c }
          .takeWhile{ case (_, b, c) => b < c }
          .filter{ case (a,b,c) => a+b+c == targetSum && a**2+b**2 == c**2 }
          .map{ case (a,b,c) => Vector(a, b,c) }
      } yield arr
    ).headOption
  }


  def groups(terms: Int)(series: Vector[Int]): Vector[Vector[Int]] = {
      (0 to series.length - terms)
        .toVector
        .map(startIndex => series.slice(startIndex, startIndex + terms))
  }

  def transpose(grid: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    grid.head.indices.toVector.map {
      col => grid.indices.toVector.map {
        row => grid(row)(col)
      }
    }
  }

  case class Point(x: Int, y: Int)

  def incrementedDiagonal(xss: Vector[Vector[Int]], start: Point): Vector[Int] = {
    @tailrec def inner(p: Point, acc: List[Int]): List[Int] = {
      p match {
        case Point(i, j) if j >= xss.length -start.y || i >= xss.length -start.x  => acc
        case Point(i, j) => inner(Point(i+1, j+1), acc :+ xss(j+start.y)(i+start.x))
      }
    }
    inner(Point(0,0), Nil).toVector
  }


  def decrementedYDiagonal(xss: Vector[Vector[Int]], start: Point): Vector[Int] = {
    @tailrec def inner(p: Point, acc: List[Int]): List[Int] = {
      p match {
        case Point(i, j) if start.y + j < 0 || i >= xss.length - start.x  => acc
        case Point(i, j) => inner(Point(i+1, j-1), acc :+ xss(j+start.y)(i+start.x))
      }
    }
    inner(Point(0, 0), Nil).toVector
  }

  def allIncDiag(grid: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    Vector(
      grid.indices.reverse.init.map{ j => incrementedDiagonal(grid, Point(0, j)) },
      grid.indices.map { i => incrementedDiagonal(grid, Point(i, 0)) }
    ).flatten
  }

  def allDecDiag(grid: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    Vector(
      grid.indices.map { j => decrementedYDiagonal(grid, Point(0, j)) },
      grid.indices.tail.map { i => decrementedYDiagonal(grid, Point(i, grid.length - 1))}
    ).flatten
  }

  def productInGrid(grid: Vector[Vector[Int]], terms: Int): (Vector[Int], Int) = {
    Vector(
      grid,
      transpose(grid),
      allIncDiag(grid),
      allDecDiag(grid)
    ).flatten
      .filter(_.length >= terms)
      .flatMap(groups(terms))
      .map(v => (v, v.product))
      .maxBy{case(_, p) => p }
  }



  def triangularNumber(n: Int): Int = {
    n * (n + 1) / 2
  }

  def highlyDivisibleTriangular(minDivisorCount: Int)(implicit pfc: PrimeFactorContext): Long = {
    Stream.from(1)
      .map(triangularNumber)
      .map(t => (t, pfc.divisorCount(t)))
      .dropWhile{ case (_, d) => d < minDivisorCount }
      .take(1)
      .head._1
  }

  @tailrec def sumBigInts(bigInts: Vector[String]): String = {
    val intMatrix = bigInts
      .map(_.toCharArray.map(_.toString.toInt).toVector.reverse)

    intMatrix.head.indices.toVector
      .map { i => intMatrix.flatMap(_.lift(i)).sum }

    match {
      case sums if sums.forall(_ <= 9) => sums.reverse.mkString
      case sums => sumBigInts(intermediateSum(sums))
    }
  }

  private def intermediateSum(sums: Vector[Int]): Vector[String] = {
    val bufferSize = sums.indices.map(i => sums(i).toString.length + i).max
    sums
      .map(_.toString.toCharArray.map(_.toString.toInt).toVector.reverse)
      .zipWithIndex
      .map { case (vectorOfInt, position) =>
        val buffer = mutable.ArrayBuffer.fill(bufferSize)(0)
        vectorOfInt.indices.foreach(idx => buffer.update(idx + position, vectorOfInt(idx)))
        buffer.toVector.reverse.mkString
      }
  }

  case class CollatzContext() {
    private val collatzLengths: mutable.Map[Long, Int] = mutable.Map(1L -> 0)

    @tailrec final def tailrecCollatzLength(n: Long, acc: Int = 0): Int = {
      n match {
          case 1 => acc + 1
          case i if i % 2 == 0 => tailrecCollatzLength(n / 2, acc + 1)
          case _ => tailrecCollatzLength(3 * n + 1, acc + 1)
        }
    }

    final def collatzLength(start: Long): Int = {
      var length = 0
      var curr = start
      var matchFound = false
      while (curr > 1 && !matchFound) {
        length = collatzLengths.get(curr) match {
          case Some(matchedLength) =>
            matchFound = true
            length + matchedLength
          case None =>
            curr = if (curr % 2 == 0) curr / 2 else 3 * curr + 1
            length + 1
        }
      }
      collatzLengths.put(start, length)
      length
    }

    final def RECcollatzLength(n: Long, acc: Int = 0): Int = {
      collatzLengths.get(n) match {
        case Some(length) => println(s"match found: $n => $length"); acc + length
        case None => val length = n match {
            case 1 => acc
            case i if i % 2 == 0 => RECcollatzLength(n / 2, acc + 1)
            case _ => RECcollatzLength(3 * n + 1, acc + 1)
          }
          collatzLengths.put(n, length)
          length
      }
    }

    def longestCollatzChain(startUpperBound: Int): Int = {
      Stream.from(1)
        .takeWhile(_ < startUpperBound)
        .maxBy(n => collatzLength(n))
    }

   // private val collatzCache: mutable.Map[Int, Int => Int] = mutable.Map(1 -> 0)
//
//    case class Collatz(n: Int, next: Option[Int] = None) {
//      lazy val len = 1
//    }

  }

  def powerDigitSum(n: Int, p: Int): Int = {
    BigInt(n).pow(p).toString.map(_.asDigit).sum
  }

  val Under20 = Vector(0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8)
  val Tens = Vector(0, 0, 6, 6, 5, 5, 5, 7, 6, 6)
  val Hundred = 7
  val Thousand = 8
  val And = 3

  case class Div(q: Int, m: Int)
  object Div{
    def of(n: Int, d: Int): Div = {
      Div(n/d, n%d)
    }
  }


  @tailrec def toLetterCount(acc: Int = 0)(i: Int): Int = {
    if (i >= 1000) {
      Div.of(i, 1000) match {
        case Div(q, 0) => acc + Under20(q) + Thousand
        case Div(q, m) => toLetterCount(acc + Under20(q) + Thousand)(m)
      }
    } else if (i >= 100) {
      Div.of(i, 100) match {
        case Div(q, 0) => acc + Under20(q) + Hundred
        case Div(q, m) => toLetterCount(acc + Under20(q) + Hundred + And)(m)
      }
    } else if (i >= 20) {
      Div.of(i, 10) match {
        case Div(q, 0) => acc + Tens(q)
        case Div(q, m) => toLetterCount(acc + Tens(q))(m)
      }
    } else acc + Under20(i)

  }

  def numberLetterCount(from: Int, to: Int): Int = {
    (from to to).map(toLetterCount()).sum
  }

  def maximumPathSumString(triangle: Vector[String]): Int = {
    maximumPathSum(triangle.map(_.split(" ").toVector.map(_.toInt)))
  }

  private def maximumPathSum(triangle: Vector[Vector[Int]]): Int = {
    maximumPathSum(triangle.init, triangle.last)
  }

  @tailrec private def maximumPathSum(triangle: Vector[Vector[Int]], bottom: Vector[Int]): Int = {
    if (bottom.length == 1) bottom.head
    else {
      val updatedBottom = bottom
        .zip(bottom.tail)
        .map { case (a, b) => math.max(a, b) }
        .zip(triangle.last)
        .map { case (a, b) => a + b }
      maximumPathSum(triangle.init, updatedBottom)
    }
  }


  val MonthLengths = Vector(31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  def isLeapYear(y: Int): Boolean = {
    if (y % 100 == 0) (y / 100) % 4 == 0
    else y % 4 == 0
  }

  def countingSundays: Int = {
    val allMonthsLengths = for {
      y <- 1900 to 2000
      m <- 1 to 12
    } yield if (m == 2) if (isLeapYear(y)) 29 else 28 else MonthLengths(m - 1)

    allMonthsLengths
      .scanLeft(1){(acc, curr) => (acc + curr) % 7 }
      .slice(12, 1212)
      .count(_ == 0)
  }

  @tailrec def factorial(n: BigInt, acc: BigInt = 1): BigInt = {
    if (n < 2) acc else factorial(n - 1, acc * n)
  }

  def factorialDigitSum(n: BigInt): Int = {
    factorial(n).toString.map(_.asDigit).sum
  }

  def properDivisorSum(n: Int): Int = {
    val (limit: Int, step: Int) = if (n % 2 == 0) (n / 2, 1) else (n/3, 2)
    (1 to limit by step)
      .collect { case i if n%i == 0 => i }
      .sum
  }

  def properDivisorSums(till: Int): IndexedSeq[Int] = {
    (0 to till).map(properDivisorSum)
  }

  def amicableNumbers(till: Int): IndexedSeq[(Int, Int)] = {
    val sums = (0 to till).zip(properDivisorSums(till))
    sums.filter{case (i, s) => i!=s && sums.lift(s).map(_._2).contains(i) }
  }

  def amicableNumbersSum(till: Int): Int = {
    amicableNumbers(till).map(_._2).sum
  }

  def namesScoresSum: Int = {
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream("p022_names.txt"))
      .getLines.toList.head.split(",")
      .map(n => n.slice(1, n.length-1))
      .sorted
      .map(_.toCharArray.map(_-64).sum)
      .zipWithIndex
      .map{case (v, i) => v *(i+1)}
      .sum
  }

  def sumOfNonAbundantSums: Int = {
    val LastNonAbundantSum = 28123
    val abundants = (0 to LastNonAbundantSum)
      .map(properDivisorSum)
      .zipWithIndex
      .collect{ case (s, i) if s > i => i}

    val abundantSums = for {
                               i <- abundants
      j <- abundants.dropWhile(_ < i).takeWhile(_ <= LastNonAbundantSum - i)
    } yield i + j

    (1 to LastNonAbundantSum).diff(abundantSums).sum
  }

  def lexicographicPermutations(s: String): Iterator[String] = {
    if (s.length == 1) Iterator(s)
    else
      for {
      c <- s.toIterator
      p <- lexicographicPermutations(s.filterNot(_ == c))
    } yield c + p
  }

  def fibonacciWithSize(size: Int): Int = {
    fibonacci
      .zipWithIndex
      .dropWhile{ case (f, _) => f.toString.length < size}
      .map{ case (_, i) => i }
      .head
  }

  def longestReciprocalCycles(below: Int): Int = {
    def cycleLength(denominator: Int): Int = {
      @tailrec def cl(r: Int = 10, acc: Int = 0): Int = {
        if (r != 10 || acc < 1)
          cl(r % denominator * 10, acc + 1)
        else acc
      }
      cl()
    }

    (2 to below)
      .filterNot(i => i % 2 == 0 || i % 5 == 0)
      .map(i => (i, cycleLength(i)))
      .maxBy {case (_, l) => l}
      ._1
  }

  def quadraticPrimes(absLimit: Int): Int = {
    val pfc = PrimeFactorContext(absLimit)
    val (a, b, primeCount) = (for {
      b <- pfc.primes
      a <- 1 - absLimit to absLimit by 2
    } yield (a, b)
    ).map{ case (a, b) =>
      val primeCount = Stream.from(0)
        .map { n => n * n + a * n + b }
        .takeWhile(pfc.primes.contains)
        .length
      (a, b, primeCount)
    }
    .maxBy{ case (_, _, primeCount) => primeCount }

    println(s"Res: a= $a b= $b primes= $primeCount")

    a*b
  }

  def numberSpiralDiagonals(width: Int): Int = {
    val layers = width / 2
    @tailrec def f(start: Int, step: Int, acc: Int = 0, layer: Int = 1): Int = {
      if (layer <= layers)
        f(
          start + 4 * step,
          step + 2,
          acc + 4 * start + 10 * step,
          layer + 1
        )
      else acc
    }
    1 + f(1, 2)
  }

  type MatrixBuffer[T] = ArrayBuffer[ArrayBuffer[T]]

  implicit class MatrixBufferExt[T](m: MatrixBuffer[T]) {
    def set(x: Int, y: Int, v: T): Unit = {
      m(y).update(x, v)
    }
  }

  sealed trait Direction {
    def nextCoordinates(x: Int, y: Int): (Int, Int)
    val reverse: Direction
  }
  case object N extends Direction {
    override def nextCoordinates(x: Int, y: Int): (Int, Int) = (x, y - 1)
    override val reverse: Direction = S
  }
  case object E extends Direction {
    override def nextCoordinates(x: Int, y: Int): (Int, Int) = (x + 1, y)
    override val reverse: Direction = W
  }
  case object S extends Direction {
    override def nextCoordinates(x: Int, y: Int): (Int, Int) = (x, y + 1)
    override val reverse: Direction = N
  }
  case object W extends Direction {
    override def nextCoordinates(x: Int, y: Int): (Int, Int) = (x - 1, y)
    override val reverse: Direction = E
  }

  val Directions: List[Direction] = List(E, S, W, N)



  @tailrec def moves(values: List[Int], step: Int = 1, stepsToDo: Int = 1, directions: List[Direction] = Nil, acc: List[Direction] = Nil): List[Direction] = {
    val dirs: List[Direction] = if (directions == Nil) Directions else directions
    values match {
      case Nil => acc
      case _ :: tail => if (stepsToDo == 1) {
        val nextStep = if (List(N, S).contains(dirs.head)) step + 1 else step
        moves(tail, nextStep, nextStep, dirs.tail, dirs.head +: acc)
      } else moves(tail, step, stepsToDo - 1, dirs, dirs.head +: acc)
    }
  }

  def nextMoves(values: List[Int]): List[Direction] = {
    moves(values).reverse
  }

  def buildSpiral(width: Int): Vector[Vector[Int]] = {
    val matrix: MatrixBuffer[Int] = ArrayBuffer.fill(width)(ArrayBuffer.fill(width)(0))

    val (x, y) = (width/2, width/2)
    matrix.set(x, y, 1)

    @tailrec def draw(l: List[(Direction, Int)], x0: Int, y0: Int): Unit = {
      l match {
        case Nil => ()
        case (d, v) :: tail =>
          val (x, y) = d.nextCoordinates(x0, y0)
          matrix.set(x, y, v)
          draw(tail, x, y)
      }
    }

    if (width > 1) {
      val values = (2 to width * width).toList
      val directions = time(nextMoves(values), "directions")
      time(draw(directions.zip(values), x, y), "draw")
    }

    matrix.map(_.toVector).toVector
  }

  def distinctPowers(from: Int, to: Int): Int = {
    (for {
      a <- (from to to).map(BigInt(_))
      b <- from to to
    } yield a.pow(b)
      ).distinct.length
  }

  def coinsSums(target: Int, coinValues: List[Int]): Int = {
    def sums(target: Int, coinValues: List[Int]): Int = {
      coinValues match {
        case Nil => 0
        case head :: tail =>
          if (head > target) 0
          else if (head == target) 1
          else sums(target - head, coinValues) + sums(target, tail)
      }
    }
    sums(target, coinValues)
  }

  def iterSum(target: Int, coinValues: List[Int]): Int = {
    val b: ArrayBuffer[Int] = ArrayBuffer.fill(target+1)(0)
    b.update(0, 1)
    for {
      x <- coinValues
      i <- x to target
    } yield b.update(i, b(i)+b(i-x))
    b(target)
  }

}
