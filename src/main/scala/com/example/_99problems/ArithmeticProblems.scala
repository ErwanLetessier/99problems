package com.example._99problems


object ArithmeticProblems {

  def isPrime(n: Int): Boolean = {
    n > 1 && (2 to math.sqrt(n).toInt).forall(n % _ > 0)
  }

  def primesUntil(n: Int): Stream[Int] = Stream.from(2).filter(isPrime).takeWhile(_<= n)

  def primeFactors(n: Int): List[Int] = {
    val primes = primesUntil(n).toList
    def findPrimeFactors(n: Int, acc: List[Int] = Nil): List[Int] = {
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



}
