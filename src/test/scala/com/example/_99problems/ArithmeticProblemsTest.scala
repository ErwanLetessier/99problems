package com.example._99problems

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner
import ArithmeticProblems._

@RunWith(classOf[JUnitRunner])
class ArithmeticProblemsTest extends FunSuite with Matchers {

  def assertIsPrime(n: Int): Unit = {
    assertPrime(n, expected = true)
  }

  def assertIsNotPrime(n: Int): Unit = {
    assertPrime(n, expected = false)
  }

  def assertPrime(n: Int, expected: Boolean): Unit = {
    withClue(s"Prime($n)") { isPrime(n) shouldBe expected }
  }

  test("31 should return whether n is prime or not") {
    assertIsNotPrime(1)
    assertIsPrime(2)
    assertIsPrime(3)
    assertIsNotPrime(4)
    assertIsPrime(5)
    assertIsNotPrime(6)
    assertIsPrime(7)
    assertIsNotPrime(8)
    assertIsNotPrime(9)
    assertIsPrime(11)
    assertIsPrime(13)
    assertIsPrime(17)
  }

  def assertPrimeFactors(n: Int, expected: List[Int]): Unit = {
    withClue(s"PrimeFactors($n)") { primeFactors(n) shouldEqual expected }
  }

  test("35 should return prime factors of n") {
    assertPrimeFactors(1, Nil)
    assertPrimeFactors(2, List(2))
    assertPrimeFactors(3, List(3))
    assertPrimeFactors(4, List(2,2))
    assertPrimeFactors(5, List(5))
    assertPrimeFactors(6, List(2, 3))
    assertPrimeFactors(8, List(2,2,2))
    assertPrimeFactors(9, List(3,3))
    assertPrimeFactors(10, List(2,5))
  }

  test("32 should return greatest common divisor of a and b") {
    gcd(13, 17) shouldEqual 1
    gcd(20536, 7826) shouldEqual 2
    gcd(36, 48) shouldEqual 12
    gcd(240, 90) shouldEqual 30
  }

  def assertAreCoprime(a: Int, b: Int) = {
    assertCoprime(a, b, expected = true)
  }

  def assertAreNotCoprime(a: Int, b: Int) = {
    assertCoprime(a, b, expected = false)
  }

  def assertCoprime(a: Int, b: Int, expected: Boolean) = {
    withClue(s"coprime($a, $b)") { coprime(a, b) shouldBe expected }
  }

  test("33 should return whether a anb are co-prime or not") {
    assertAreCoprime(13, 27)
    assertAreNotCoprime(20536, 7826)
  }

  test ("34 should return euler totient: number of integer with 1 <=n < m and n coprime m") {
    eulerTotient(10) shouldEqual 4
    eulerTotient(13) shouldEqual 12
  }

  test("36 should return prime factors cardinality") {
    primeFactorsCardinality(315) shouldEqual List((3,2), (5,1), (7,1))
  }

  test("37 should return euler's totient using cardinality") {
    cardinalEulerTotient(10) shouldEqual 4
    cardinalEulerTotient(13) shouldEqual 12
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  test(" time both euler's totient") {
    time(cardinalEulerTotient(10090))
    //time(eulerTotient(10090))
    /* obviously applying math formula is much more efficient than iterative brute force */
  }

  test("39 should return all primes between a and b") {
    allPrimes(2, 7920) should have size 1000
  }
}
