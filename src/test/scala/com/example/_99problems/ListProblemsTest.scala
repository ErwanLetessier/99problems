package com.example._99problems

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import ListProblems._

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class ListProblemsTest extends FunSuite with Matchers {

  test("01 should return last element of the list") {
    last(List()) shouldBe None
    last(List(1)) shouldBe Some(1)
    last(List(1, 2)) shouldBe Some(2)
  }

  test("02 should return last 2 elements of the list") {
    lastTwo(List()) shouldBe None
    lastTwo(List(1)) shouldBe None
    lastTwo(List(1, 2)) shouldBe Some((1, 2))
  }

  test("03 should return the k th element of the list") {
    at(3, List()) shouldBe None
    at(3, List(1)) shouldBe None
    at(1, List(1)) shouldBe Some(1)
    at(2, List(4, 5, 6)) shouldBe Some(5)
  }

  test("04 should return the length of the list") {
    len(List()) shouldBe 0
    len(List(2)) shouldBe 1
    len(List(2, 5, 8, 7)) shouldBe 4
  }

  test("05 should return list in reverse order") {
    reverse(List()) shouldBe Nil
    reverse(List(1)) shouldBe List(1)
    reverse(List(1, 2)) shouldBe List(2, 1)
    reverse(List(1, 2, 3, 4, 5)) shouldBe List(5, 4, 3, 2, 1)
  }

  test("06 should return whether the list ia a palindrome") {
    isPalindrome(List()) shouldBe true
    isPalindrome(List(1)) shouldBe true
    isPalindrome(List(1, 2)) shouldBe false
    isPalindrome(List(1, 2, 2, 1)) shouldBe true
  }

  test("07 should flatten list of nested list") {
    flatten(List()) shouldBe Nil
    flatten(List(List(1))) shouldBe List(1)
    flatten(List(List(1, 2))) shouldBe List(1, 2)
    flatten(List(List(1, 2), List(3))) shouldBe List(1, 2, 3)
  }

  test("08 should compress list: eliminate consecutive duplicates") {
    compress(List()) shouldBe Nil
    compress(List(1)) shouldBe List(1)
    compress(List(2, 2)) shouldBe List(2)
    compress(List(2, 3, 2)) shouldBe List(2, 3, 2)
    compress(List(1, 2, 3)) shouldBe List(1, 2, 3)
    compress(List(1, 1, 1, 1, 2, 2, 2, 3, 1, 1, 2, 2, 3, 3)) shouldBe List(1, 2, 3, 1, 2, 3)
  }

  test("09 should pack consecutive duplicates into sub lists") {
    pack(List()) shouldBe Nil
    pack(List(1)) shouldBe List(List(1))
    pack(List(2, 2)) shouldBe List(List(2, 2))
    pack(List(1, 2, 2, 3)) shouldBe List(List(1), List(2, 2), List(3))
  }

  test("10 should return length-encoding of input list") {
    lengthEncode(List()) shouldBe Nil
    lengthEncode(List(3)) shouldBe List((1, 3))
    lengthEncode(List(2, 2, 2)) shouldBe List((3, 2))
  }

  test("11 should return named length-encoding of input list") {
    namedLengthEncode(List()) shouldBe Nil
    namedLengthEncode(List(3)) shouldBe List(One(3))
    namedLengthEncode(List(2, 2, 2)) shouldBe List(Many(3, 2))
    namedLengthEncode(List(2, 2, 2, 4, 3, 3)) shouldBe List(Many(3, 2), One(4), Many(2, 3))
  }

  test("12 should return length-decoded list") {
    decode(List()) shouldBe Nil
    decode(List(One(4))) shouldBe List(4)
    decode(List(Many(3, 2))) shouldBe List(2, 2, 2)
    decode(List(Many(3, 2), One(4), Many(2, 3))) shouldBe List(2, 2, 2, 4, 3, 3)
  }

  test("13 should return direct named length-encoding of input list") {
    directLengthEncode(List()) shouldBe Nil
    directLengthEncode(List(3)) shouldBe List(One(3))
    directLengthEncode(List(2, 2, 2)) shouldBe List(Many(3, 2))
    directLengthEncode(List(2, 2, 2, 4, 3, 3)) shouldBe List(Many(3, 2), One(4), Many(2, 3))
    directLengthEncode(List(2, 2, 2, 4, 3, 3, 6)) shouldBe List(Many(3, 2), One(4), Many(2, 3), One(6))
    directLengthEncode(List(5, 2, 2, 2, 4, 3, 3)) shouldBe List(One(5), Many(3, 2), One(4), Many(2, 3))
  }

  test("14 should duplicate all elements of input list") {
    duplicate(List()) shouldBe Nil
    duplicate(List(4)) shouldBe List(4, 4)
    duplicate(List(4, 5, 6)) shouldBe List(4, 4, 5, 5, 6, 6)
  }

  test("15 should replicate all elements of input list n times") {
    replicate(3, List()) shouldBe Nil
    replicate(2, List(4)) shouldBe List(4, 4)
    replicate(4, List(5)) shouldBe List(5, 5, 5, 5)
    replicate(2, List(4, 5, 6)) shouldBe List(4, 4, 5, 5, 6, 6)
  }

  test("16 should drop n th element form the list") {
    drop(2, List()) shouldBe Nil
    drop(2, List(1)) shouldBe List(1)
    drop(1, List(9)) shouldBe Nil
    drop(1, List(5, 6)) shouldBe List(6)
    drop(3, List(9, 8, 7, 6, 5, 4)) shouldBe List(9, 8, 6, 5, 4)
  }

  test("17 should return list split into n and remaining") {
    split(3, List()) shouldBe(Nil, Nil)
    split(3, List(1)) shouldBe(List(1), Nil)
    split(3, List(1, 2, 3, 4)) shouldBe(List(1, 2, 3), List(4))
  }

  test("18 should return splice from i th to k th element out of list") {
    slice(1, 1, List()) shouldBe Nil
    slice(1, 2, List(3)) shouldBe List(3)
    slice(1, 1, List(2)) shouldBe List(2)
    slice(1, 1, List(2, 3)) shouldBe List(2)
    slice(3, 6, List(2, 3, 4, 5, 6, 7, 8, 9)) shouldBe List(4, 5, 6, 7)
    slice(3, 12, List(2, 3, 4, 5, 6, 7, 8, 9)) shouldBe List(4, 5, 6, 7, 8, 9)
  }

  test("should return the n first elements of the list") {
    left(2, List()) shouldBe Nil
    left(1, List(5)) shouldBe List(5)
    left(2, List(5)) shouldBe List(5)
    left(2, List(5, 6, 7, 8)) shouldBe List(5, 6)
  }

  test("should return the n last elements of the list") {
    right(2, List()) shouldBe Nil
    right(1, List(5)) shouldBe List(5)
    right(2, List(5)) shouldBe List(5)
    right(2, List(5, 6, 7, 8)) shouldBe List(7, 8)
  }

  test("19 should return list rotated by n places to the left") {
    rotateLeft(2, List()) shouldBe Nil
    rotateLeft(1, List(5)) shouldBe List(5)
    rotateLeft(2, List(5)) shouldBe List(5)
    rotateLeft(1, List(4, 5)) shouldBe List(5, 4)
    rotateLeft(2, List(4, 5)) shouldBe List(4, 5)
    rotateLeft(3, List(4, 5)) shouldBe List(5, 4)
    rotateLeft(2, List(5, 6, 7)) shouldBe List(7, 5, 6)
    rotateLeft(1, List(5, 6, 7)) shouldBe List(6, 7, 5)
    rotateLeft(2, List(5, 6, 7, 8, 9)) shouldBe List(7, 8, 9, 5, 6)
    rotateLeft(-2, List(5, 6, 7, 8, 9)) shouldBe List(8, 9, 5, 6, 7)
  }

  test("19-1 should return list rotated by n places to the right") {
    rotateRight(2, List()) shouldBe Nil
    rotateRight(1, List(5)) shouldBe List(5)
    rotateRight(2, List(5)) shouldBe List(5)
    rotateRight(1, List(4, 5)) shouldBe List(5, 4)
    rotateRight(2, List(4, 5)) shouldBe List(4, 5)
    rotateRight(3, List(4, 5)) shouldBe List(5, 4)
    rotateRight(2, List(5, 6, 7)) shouldBe List(6, 7, 5)
    rotateRight(1, List(5, 6, 7)) shouldBe List(7, 5, 6)
    rotateRight(2, List(5, 6, 7, 8, 9)) shouldBe List(8, 9, 5, 6, 7)
    rotateRight(-2, List(5, 6, 7, 8, 9)) shouldBe List(7, 8, 9, 5, 6)
  }

  test("20 should remove n th element") {
    removeAt(1, List()) shouldBe Nil
    removeAt(1, List(4)) shouldBe Nil
    removeAt(2, List(6)) shouldBe List(6)
    removeAt(2, List(1, 2, 3, 4)) shouldBe List(1, 3, 4)
  }

  test("21 should insert value at n th position (starting from 1)") {
    insertAt(1, 99, List()) shouldBe List(99)
    insertAt(1, 99, List(5, 8)) shouldBe List(99, 5, 8)
    insertAt(2, 99, List(5, 8)) shouldBe List(5, 99, 8)
    insertAt(5, 99, List(5, 8)) shouldBe List(5, 8, 99)
    insertAt(-4, 99, List(5, 8)) shouldBe List(99, 5, 8)
  }

  test("22 should return a list containing all integers in range") {
    range(1, 1) shouldBe List(1)
    range(7, 12) shouldBe List(7, 8, 9, 10, 11, 12)
    range(11, 8) shouldBe List(11, 10, 9, 8)
  }


  private def stableRandom: Random = new Random(101)

  test("23 should return a list of n elements randomly selected from the list") {
    randomSelect(1, List(), stableRandom) shouldBe Nil
    randomSelect(1, List(5), stableRandom) shouldBe List(5)
    randomSelect(3, List(1, 2, 3, 4, 5, 6, 7), stableRandom) shouldBe List(2, 6, 3)
  }

  test("24 should return n unique random elements out of a list of M elements (Lotto)") {
    lotto(6, List(), stableRandom) shouldBe Nil
    val lottoNumbers = (1 to 49).toList
    lotto(1, lottoNumbers, stableRandom) shouldBe List(2)
    lotto(2, lottoNumbers, stableRandom) shouldBe List(2, 12)
    lotto(6, lottoNumbers, stableRandom) shouldBe List(2, 12, 10, 40, 29, 13)
  }

  test("25 should return random permutations of elements of input list") {
    permutation(List(), stableRandom) shouldBe Nil
    permutation(List(4), stableRandom) shouldBe List(4)
    permutation(List(5, 9), stableRandom) shouldBe List(9, 5)
    permutation(List(1, 2, 3, 4, 5, 6), stableRandom) shouldBe List(5, 1, 3, 4, 2, 6)
  }

  test("26 should return combination C(n,p) of p elements out of a list of n elements") {
    combinations(2, List()) should contain theSameElementsAs List()
    combinations(1, List(5)) should contain theSameElementsAs  List(List(5))
    combinations(1, List(5, 6)) should contain theSameElementsAs List(List(5), List(6))
    combinations(2, List(5, 6, 7)) should contain theSameElementsAs List(List(5, 6), List(5, 7), List(6, 7))
  }

  test ("27 should return combination C(n,p) for all x from 1 to p") {
    groups(2, List()) should contain theSameElementsAs List()
    groups(1, List(5)) should contain theSameElementsAs List(List(5))
    groups(1, List(5, 6)) should contain theSameElementsAs List(List(5), List(6))
    groups(2, List(5, 6)) should contain theSameElementsAs List(List(5), List(6), List(5, 6))
    groups(2, List(5, 6, 7)) should contain theSameElementsAs List(List(5), List(6), List(7), List(5, 6), List(5, 7), List(6, 7))
    groups(3, List(5, 6, 7)) should contain theSameElementsAs List(List(5), List(6), List(7), List(5, 6), List(5, 7), List(6, 7), List(5, 6, 7))
  }

  test ("27 bis should return combination C(n,p) for all x < n in a list of Ps (generalization)") {
    specifiedGroups(List(2), List()) should contain theSameElementsAs List()
    specifiedGroups(List(1), List(5)) should contain theSameElementsAs List(List(5))
    specifiedGroups(List(1), List(5, 6)) should contain theSameElementsAs List(List(5), List(6))
    specifiedGroups(List(2), List(5, 6)) should contain theSameElementsAs List(List(5, 6))
    specifiedGroups(List(2), List(5, 6, 7)) should contain theSameElementsAs List(List(5, 6), List(5, 7), List(6, 7))
    specifiedGroups(List(3), List(5, 6, 7)) should contain theSameElementsAs List(List(5, 6, 7))
    specifiedGroups(List(1, 3), List(5, 6, 7)) should contain theSameElementsAs List(List(5), List(6), List(7), List(5, 6, 7))
  }

}
