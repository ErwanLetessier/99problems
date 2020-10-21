package com.euler.project


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}
import Euler._
import org.scalatest.Inside.inside

@RunWith(classOf[JUnitRunner])
class EulerTest extends FunSuite with Matchers {

  test("2 even fibonacci number sum") {
    evenFibonacciSum(50) shouldEqual 44
    //evenFibonacciSum(4000000) shouldEqual 4613732
  }

  test("3 largest prime factor") {
    largestPrimeFactor(13195) shouldEqual 29
    //largestPrimeFactor(600851475143L) shouldEqual 6857
  }

  test("4 largest palindrome product") {
    largestPalindromeProduct(100, 999) shouldEqual (913, 993)
  }

  test("5 smallest multiple") {
    smallestMultiple(1, 10) shouldEqual 2520
    smallestMultiple(1, 20) shouldEqual 232792560
  }

  test("6 sum square difference") {
    sumSquareDifference(1, 10) shouldBe 2640
    sumSquareDifference(1, 100) shouldBe 25164150
  }

  test("7 10001st prime") {
    nthPrime(6) shouldEqual 13
    nthPrime(10001) shouldEqual 104743
  }

  val Series =
    "73167176531330624919225119674426574742355349194934" +
    "96983520312774506326239578318016984801869478851843" +
    "85861560789112949495459501737958331952853208805511" +
    "12540698747158523863050715693290963295227443043557" +
    "66896648950445244523161731856403098711121722383113" +
    "62229893423380308135336276614282806444486645238749" +
    "30358907296290491560440772390713810515859307960866" +
    "70172427121883998797908792274921901699720888093776" +
    "65727333001053367881220235421809751254540594752243" +
    "52584907711670556013604839586446706324415722155397" +
    "53697817977846174064955149290862569321978468622482" +
    "83972241375657056057490261407972968652414535100474" +
    "82166370484403199890008895243450658541227588666881" +
    "16427171479924442928230863465674813919123162824586" +
    "17866458359124566529476545682848912883142607690042" +
    "24219022671055626321111109370544217506941658960408" +
    "07198403850962455444362981230987879927244284909188" +
    "84580156166097919133875499200524063689912560717606" +
    "05886116467109405077541002256983155200055935729725" +
    "71636269561882670428252483600823257530420752963450"

  test("8 largest product in a series") {
    largestProductInSeries(Series, 4) shouldEqual ("9989", 5832)
    largestProductInSeries(Series, 13) shouldEqual ("5576689664895",23514624000L)
  }

  def assertPythagoreanTriplet(target: Int, triplet: (Int, Int, Int)): Unit = {
    val (a, b, c) = triplet
    inside (pythagoreanTriplet(target)) {
      case Some(arr: Array[Int]) => arr should contain theSameElementsAs Array(a, b, c)
    }
  }

  test("9 Pythagorean triplet") {
    assertPythagoreanTriplet(12, (3,4,5))
    assertPythagoreanTriplet(1000, (200, 375, 425))
  }

}
