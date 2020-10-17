package com.example._99problems

import scala.annotation.tailrec


object ArithmeticProblems {

  def isPrime(n: Int): Boolean = {
    n > 1 && (2 to math.sqrt(n).toInt).forall(n % _ > 0)
  }

  def primesUntil(n: Int): Stream[Int] = Stream.from(2).filter(isPrime).takeWhile(_<= n)

  def primeFactors(n: Int): List[Int] = {
    val primes = primesUntil(n).toList
    @tailrec def findPrimeFactors(n: Int, acc: List[Int] = Nil): List[Int] = {
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

  def gcd(a: Int, b: Int): Int = {
    val aFactors = 1 +: primeFactors(a)
    val bFactors = 1 +: primeFactors(b)
    (aFactors intersect bFactors).product
  }

  def coprime(a: Int, b: Int): Boolean = {
    gcd(a, b) == 1
  }

  implicit class IntExtension(i: Int) {
    def coprime(j: Int): Boolean = {
      ArithmeticProblems.coprime(i, j)
    }
  }

  def eulerTotient(m: Int): Int = {
    (1 until m).count(m.coprime)
  }

  def primeFactorsCardinality(m: Int): List[(Int, Int)] = {
    (
      primeFactors _
        andThen ListProblems.pack
      )(m)
      .map(e => (e.head, e.length)
    )
  }

  def cardinalEulerTotient(m: Int): Int = {
    primeFactorsCardinality(m)
      .map { case (prime, multiplicity) => (prime - 1) * math.pow(prime, multiplicity -1).toInt }
      .product
  }

  def allPrimes(low: Int, high: Int): Stream[Int] = {
    Stream.from(low)
      .filter(isPrime)
      .takeWhile(_ <= high)
  }


}
