package com.example._99problems

import scala.annotation.tailrec
import scala.collection.GenTraversableOnce

object ListProblems {

  @tailrec
  def last[A](list: List[A]): Option[A] = {
    list match {
      case Nil => None
      case head :: Nil => Some(head)
      case _ :: tail => last(tail)
    }
  }

  @tailrec
  def lastTwo[A](list: List[A]): Option[(A, A)] = {
    list match {
      case Nil => None
      case _ :: Nil => None
      case b :: a :: Nil => Some((b, a))
      case _ :: a :: tail => lastTwo(a :: tail)
    }
  }

  @tailrec
  def at[A](k: Int, list: List[A]): Option[A] = {
    list match {
      case Nil => None
      case head :: _ if k == 1 => Some(head)
      case _ :: tail => at(k - 1, tail)
    }
  }


  def len[A](list: List[A]): Int = {
    @tailrec def tailrecLength(l: List[A], acc: Int): Int = {
      l match {
        case Nil => acc
        case _ :: tail => tailrecLength(tail, acc + 1)
      }
    }
    tailrecLength(list, 0)
  }

  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def tailRecReverse[A](list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: Nil => head :: acc
        case head :: tail => tailRecReverse(tail, head :: acc)
      }
    }
    tailRecReverse(list, Nil)
  }

  def isPalindrome[A](list: List[A]): Boolean = {
    list == reverse(list)
  }

  def flatten[A](list: List[List[A]]): List[A] = {
    @tailrec def tailRecFlatten[A](list: List[List[A]], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail => tailRecFlatten(tail, acc ++ head)
      }
    }
    tailRecFlatten(list, Nil)
  }

  def compress[A](list: List[A]): List[A] = {
    @tailrec def tailRecCompress[A](list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail if acc == Nil => tailRecCompress(tail, head :: acc)
        case head :: tail if acc.head == head => tailRecCompress(tail, acc)
        case head :: tail  => tailRecCompress(tail, head :: acc)
      }
    }
    reverse(tailRecCompress(list, Nil))
  }



  def pack[A](list: List[A]): List[List[A]] = {
    @tailrec def tailRecPack(list: List[A], acc: List[List[A]]): List[List[A]] = {
      list match {
        case Nil => acc
        case head :: tail => acc match {
          case Nil => tailRecPack (tail, List (head) :: acc)
          case h :: t if h.head == head => tailRecPack(tail, (head :: h) :: t)
          case _ => tailRecPack(tail, List(head) :: acc)
        }
      }
    }
    reverse(tailRecPack(list, Nil))
  }

  def lengthEncode[A](list: List[A]): List[(Int, A)] = {
    pack(list).map(e => (e.length, e.head))
  }

  private def toEncoded[A](lists: List[List[A]]): List[Encoded[A]] = {
    @tailrec def tailRecToEncoded[A](lists: List[List[A]], acc: List[Encoded[A]]): List[Encoded[A]] = {
      lists match {
        case Nil => acc
        case list :: tail => list match {
          case Nil => acc
          case head :: Nil => tailRecToEncoded(tail, acc ++ List(One(head)))
          case head :: _ => tailRecToEncoded(tail, acc ++ List(Many(list.length, head)))
        }
      }
    }
    tailRecToEncoded(lists, Nil)
  }

  def namedLengthEncode[A](list: List[A]): List[Encoded[A]] = {
    toEncoded(pack(list))
  }

  private def fillList[A](times: Int, value: A): List[A] = {
    @tailrec def tailRecFillList[A](times: Int, value: A, acc: List[A]): List[A] = {
      if (times > 0) tailRecFillList(times - 1, value, value :: acc)
      else acc
    }
    tailRecFillList(times, value, Nil)
  }

  private def decoded[A](encoded: Encoded[A]): List[A] = {
    encoded match {
      case One(value) => List(value)
      case Many(times, value) => fillList(times, value)
    }
  }

  def decode[A](encoded: List[Encoded[A]]): List[A] = {
    @tailrec def tailRecDecode[A](encoded: List[Encoded[A]], acc: List[A]): List[A] = {
      encoded match {
        case Nil => acc
        case head :: tail =>  tailRecDecode(tail, acc ++ decoded(head))
      }
    }
    tailRecDecode(encoded, Nil)
  }

  def directLengthEncode[A](list: List[A]): List[Encoded[A]] = {
    @tailrec def tailRecDirectLengthEncode[A](list: List[A], acc: List[Encoded[A]]): List[Encoded[A]] = {
      list match {
        case Nil => acc
        case value :: tail => tailRecDirectLengthEncode(tail, nextAccValue(acc, value))
      }
    }
    reverse(tailRecDirectLengthEncode(list, Nil))
  }

  private def nextAccValue[A](acc: List[Encoded[A]], value: A): List[Encoded[A]] = {
    acc match {
      case Nil => List(One(value))
      case One(v) :: tail if v == value => Many(2, value) :: tail
      case One(_) :: _ => One(value) :: acc
      case Many(times, v) :: tail if v == value => Many(times + 1, v) :: tail
      case Many(_, _) :: _  => One(value) :: acc
    }
  }

  def duplicate[A](list: List[A]): List[A] = {
    @tailrec def tailRecDuplicate[A](list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail => tailRecDuplicate(tail, acc ++ List(head, head))
      }
    }
    tailRecDuplicate(list, Nil)
  }

  def replicate[A](times: Int, list: List[A]): List[A] = {
    @tailrec def tailRecReplicate[A](list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail => tailRecReplicate(tail, acc ++ fillList(times, head))
      }
    }
    tailRecReplicate(list, Nil)
  }



}


