package com.example._99problems

import org.scalatest.FunSuite
import org.scalatest.Matchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import ListProblems._

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
    rotateLeft(2, List(5)) shouldBe List(5)
    rotateLeft(1, List(4, 5)) shouldBe List(5, 4)
    rotateLeft(2, List(4, 5)) shouldBe List(4, 5)
    rotateLeft(3, List(4, 5)) shouldBe List(5, 4)
    rotateLeft(2, List(5, 6, 7)) shouldBe List(7, 5, 6)
    rotateLeft(1, List(5, 6, 7)) shouldBe List(6, 7, 5)
    rotateLeft(2, List(5, 6, 7, 8, 9)) shouldBe List(7, 8, 9, 5, 6)
  }

}

