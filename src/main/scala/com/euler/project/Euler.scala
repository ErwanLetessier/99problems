package com.euler.project

import com.example._99problems.ArithmeticProblems
import com.example._99problems.ArithmeticProblems.isPrime

import scala.annotation.tailrec


object Euler {

  def fibonacci: Stream[Int] = {
    def nextFibonacci(n2: Int, n1: Int): Stream[Int] = {
      val n = n2 + n1
      n #:: nextFibonacci(n1, n)
    }
    0 #:: 1 #:: nextFibonacci(0,1)
  }

  def evenFibonacciSum(until: Int): Long = {
    fibonacci
      .filter(_ % 2 == 0)
      .takeWhile(_ < until)
      .sum
  }


  def primesUntil(n: Long): Stream[Long] = Stream.from(2).filter(isPrime).takeWhile(_<= n).map(_.toLong)

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
    (a to b)
      .filter(_ > 1)
      .flatMap(ArithmeticProblems.primeFactorsCardinality)
      .groupBy{ case (prime, _) => prime }
      .map{ case (prime, l) => (prime, l.map{case(_, c) => c}.max) }
      .map { case (prime, multiplicity) => math.pow(prime, multiplicity).toInt }
      .product
  }

  def sum(seq: Seq[Int]) = {
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

  def pythagoreanTriplet(targetSum: Int): Option[Array[Int]] = {
    (
      for {
        a <- 1 until targetSum
        b <- a to targetSum - a
      } yield (a, b)
    )
    .flatMap { case (a, b) => intHypothenus(a, b).map(c => Array(a, b, c))}
    .find(_.sum == targetSum)
  }

}
