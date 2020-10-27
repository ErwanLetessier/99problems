package com.example._99problems

import scala.annotation.tailrec
import com.example._99problems.ArithmeticProblems.PrimeFactorContext._

import scala.collection.mutable

object ArithmeticProblems {

  object PrimeFactorContext {
    def primesUntil(n: Int): Stream[Int] = Stream.from(2).filter(isPrime).takeWhile(_<= n)

    def calculatePrimesStream(end: Int): List[Int] = {
      val intSqrt = Math.sqrt(end).toInt
      val odds = Stream.from(3, 2).takeWhile(_ <= intSqrt)
      val composites = odds.flatMap(i => Stream.from(i * i, 2 * i).takeWhile(_ <= end))
      Stream.from(3, 2).takeWhile(_ <= end).diff(composites).toList
    }

    def primesIterative(end: Int): List[Int] = {
      val primeIndices = mutable.ArrayBuffer.fill((end + 1) / 2)(true)

      val intSqrt = Math.sqrt(end).toInt
      for (i <- 3 to end by 2 if i <= intSqrt) {
        for (nonPrime <- i * i to end by 2 * i) {
          primeIndices.update(nonPrime / 2, false)
        }
      }

      2 +: primeIndices.indices
        .filter(primeIndices)
        .map(_ * 2 + 1)
        .tail.toList
    }
  }

  case class PrimeFactorContext(until: Int) {
    val primes = primesIterative(until) //primesUntil(until).toList

    def primeFactors(n: Int): List[Int] = {
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

    def primeFactorsCardinality(m: Int): List[(Int, Int)] = {
      (
        primeFactors _
          andThen ListProblems.pack
        )(m)
        .map(e => (e.head, e.length)
        )
    }

    def divisorCount(n: Int): Int = {
      primeFactorsCardinality(n).map { case (_, c) => c + 1 }.product
    }
  }

  def isPrime(n: Int): Boolean = {
    n > 1 && (2 to math.sqrt(n).toInt).forall(n % _ > 0)
  }


  def gcd(a: Int, b: Int): Int = {
    val pfc = PrimeFactorContext(List(a,b).max)
    val aFactors = 1 +: pfc.primeFactors(a)
    val bFactors = 1 +: pfc.primeFactors(b)
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

  def cardinalEulerTotient(m: Int): Int = {
    PrimeFactorContext(m).primeFactorsCardinality(m)
      .map { case (prime, multiplicity) => (prime - 1) * math.pow(prime, multiplicity -1).toInt }
      .product
  }

  def allPrimes(low: Int, high: Int): Stream[Int] = {
    Stream.from(low)
      .filter(isPrime)
      .takeWhile(_ <= high)
  }

  object GoldbachLimit {
    def goldbach(n: Int): Option[(Int, Int)] = {
      GoldbachLimit(n).goldbach(n)
    }
    def goldbachComposition(n: Int): Option[(Int, (Int, Int))] = {
      GoldbachLimit(n).goldbachComposition(n)
    }
  }

  case class GoldbachLimit(high: Int, low: Int = 2, limit: Int = 0) {

    val primes = allPrimes(2, high).toList
    val reversePrimes = primes.reverse

    def goldbach(n: Int): Option[(Int, Int)] = {
      if (n%2 == 0) {
        primes
          .find(p => reversePrimes.contains(n-p))
          .map(p => (p, n-p))
      } else None
    }

    def goldbachComposition(n: Int): Option[(Int, (Int, Int))] = {
      goldbach(n).map((n, _))
    }

    def goldbachList: List[(Int, (Int, Int))] = {
      (low to high)
        .filter(_%2 == 0)
        .flatMap(goldbachComposition)
        .toList
    }

    def goldbachLimit: List[(Int, (Int, Int))] = {
      goldbachList
        .filter{case(_, (p1, _)) => p1 > limit }
    }
  }


}
