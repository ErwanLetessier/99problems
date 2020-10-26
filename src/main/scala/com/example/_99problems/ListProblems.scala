package com.example._99problems

import scala.annotation.tailrec
import scala.util.Random

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
    def tailRecReverse(list: List[A], acc: List[A]): List[A] = {
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
    @tailrec def tailRecFlatten(list: List[List[A]], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail => tailRecFlatten(tail, acc ++ head)
      }
    }
    tailRecFlatten(list, Nil)
  }

  def compress[A](list: List[A]): List[A] = {
    @tailrec def tailRecCompress(list: List[A], acc: List[A]): List[A] = {
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
    @tailrec def tailRecToEncoded(lists: List[List[A]], acc: List[Encoded[A]]): List[Encoded[A]] = {
      lists match {
        case Nil => acc
        case list :: tail => list match {
          case Nil => acc
          case head :: Nil => tailRecToEncoded(tail, acc :+ One(head))
          case head :: _ => tailRecToEncoded(tail, acc :+ Many(list.length, head))
        }
      }
    }
    tailRecToEncoded(lists, Nil)
  }

  def namedLengthEncode[A](list: List[A]): List[Encoded[A]] = {
    toEncoded(pack(list))
  }

  private def fillList[A](times: Int, value: A): List[A] = {
    @tailrec def tailRecFillList(times: Int, value: A, acc: List[A]): List[A] = {
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
    @tailrec def tailRecDecode(encoded: List[Encoded[A]], acc: List[A]): List[A] = {
      encoded match {
        case Nil => acc
        case head :: tail =>  tailRecDecode(tail, acc ++ decoded(head))
      }
    }
    tailRecDecode(encoded, Nil)
  }

  def directLengthEncode[A](list: List[A]): List[Encoded[A]] = {
    @tailrec def tailRecDirectLengthEncode(list: List[A], acc: List[Encoded[A]]): List[Encoded[A]] = {
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
    @tailrec def tailRecDuplicate(list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail => tailRecDuplicate(tail, acc ++ List(head, head))
      }
    }
    tailRecDuplicate(list, Nil)
  }

  def replicate[A](times: Int, list: List[A]): List[A] = {
    @tailrec def tailRecReplicate(list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail => tailRecReplicate(tail, acc ++ fillList(times, head))
      }
    }
    tailRecReplicate(list, Nil)
  }


  def drop[A](n: Int, list: List[A]): List[A] = {
    @tailrec def tailRecDrop(n: Int, list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail if n > 1 => tailRecDrop(n - 1, tail, acc :+ head)
        case _ :: tail => acc ++ tail
      }
    }
    tailRecDrop(n, list, Nil)
  }

  def split[A](length: Int, list: List[A]): (List[A], List[A]) = {
    @tailrec def tailRecSplit(length: Int, list: List[A], acc: List[A]): (List[A], List[A]) = {
      list match {
        case Nil => (acc, Nil)
        case head :: tail if length > 1 => tailRecSplit(length - 1, tail, acc :+ head)
        case head :: tail => (acc :+ head, tail)
      }
    }
    tailRecSplit(length, list, Nil)
  }

  def slice[A](i: Int, k: Int, list: List[A]): List[A] = {
    @tailrec def tailRecSlice(i: Int, k: Int, list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case _ :: tail if i > 1 => tailRecSlice(i - 1, k - 1, tail, acc)
        case head :: tail if k > 1 => tailRecSlice(i, k - 1, tail, acc :+ head)
        case head :: _ if i == k => acc :+ head
      }
    }
    tailRecSlice(i, k, list, Nil)
  }

  def left[A](n: Int, list: List[A]): List[A] = {
    @tailrec def tailRecLeft(n: Int, list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail if n > 0 => tailRecLeft(n - 1, tail, acc :+ head)
        case _ :: _ => acc
      }
    }
    tailRecLeft(n, list, Nil)
  }

  def right[A](n: Int, list: List[A]): List[A] = {
    @tailrec def tailRecRight(n: Int, list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail if n > 0 => tailRecRight(n - 1, tail, acc :+ head)
        case head :: tail if n == 0 => tailRecRight(n, tail, acc.tail :+ head)
      }
    }
    tailRecRight(n, list, Nil)
  }

  def rotateLeft[A](places: Int, original: List[A]): List[A] = {
    @tailrec def tailRecRotate(places: Int, list: List[A], acc: List[A], size: Int): List[A] = {
      list match {
        /* limits the number of full rotation > 1 and < 2 */
        case Nil if places > 0 => tailRecRotate(places % size, original, Nil, 0)
        case Nil => acc
        case head :: tail if places > 0 => tailRecRotate(places - 1, tail, acc :+ head, size + 1)
        case _ => list ++ acc
      }
    }
    original match {
      case list @ Nil => list
      case list @ _ :: Nil => list
      case list @ _ =>
        places match {
          case i if i > 0 => tailRecRotate(places, list, Nil, 0)
          case i if i < 0 => rotateRight(- places, list)
          case _ => list
        }
    }
  }

  def rotateRight[A](places: Int, original: List[A]): List[A] = {
    @tailrec def tailRecRotate(places: Int, list: List[A], keep: List[A], move: List[A], size: Int): List[A] = {
      list match {
        case Nil if places > 0 => tailRecRotate(places % size, original, Nil, Nil, 0)
        case Nil => move ++ keep
        case head :: tail if places > 0 => tailRecRotate(places - 1, tail, keep, move :+ head, size + 1)
        case head :: tail if places == 0 => tailRecRotate(places, tail, keep :+ move.head, move.tail :+ head, size + 1)
      }
    }
    original match {
      case list @ Nil => list
      case list @ _ :: Nil => list
      case list @ _ =>
        places match {
          case i if i > 0 => tailRecRotate(places, list, Nil, Nil, 0)
          case i if i < 0 => rotateLeft(- places, list)
          case _ => list
        }
    }
  }

  def removeAt[A](n: Int, list: List[A]): List[A] = {
    @tailrec def tailRecRemoveAt(n: Int, list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case head :: tail if n > 1 => tailRecRemoveAt(n - 1, tail, acc :+ head)
        case _ :: tail => acc ++ tail
      }
    }
    tailRecRemoveAt(n, list, Nil)
  }

  def insertAt[A](n: Int, value: A, list: List[A]): List[A] = {
    @tailrec def tailRecInsertAt(n: Int, value: A, list: List[A], acc: List[A]): List[A]= {
      list match {
        case Nil => acc :+ value
        case head :: tail if n > 1 => tailRecInsertAt(n - 1, value, tail, acc :+ head)
        case _ :: _ => (acc :+ value) ++ list
      }
    }
    tailRecInsertAt(n, value, list, Nil)
  }

  def range(from: Int, to: Int): List[Int] = {
    @tailrec def tailRecRange(from: Int, to: Int, acc: List[Int]): List[Int] = {
      from match {
        case _ if from < to => tailRecRange(from + 1, to, acc :+ from)
        case _ if from > to => tailRecRange(from - 1, to, acc :+ from)
        case _ => acc :+ from
      }
    }
    tailRecRange(from, to, Nil)
  }

  def randomSelect[A](n: Int, list: List[A], random: Random): List[A] = {
    val size = list.length
    @tailrec def tailRecRandomSelect(n: Int, list: List[A], acc: List[A]): List[A] = {
      if (n > 0) {
        val nextAcc = at(1 + random.nextInt(size), list) match {
          case Some(value) => acc :+ value
          case None => acc
        }
        tailRecRandomSelect(n - 1, list, nextAcc)
      } else acc
    }

    list match {
      case _ :: _ if n > 0 => tailRecRandomSelect(n, list, Nil)
      case _ => Nil
    }

  }

  def lotto[A](n: Int, numbers: List[A], random: Random): List[A] = {
    @tailrec def tailRecLotto(n: Int, numbers: List[A], random: Random, acc: List[A]): List[A] = {
      numbers match {
        case Nil => acc
        case _ =>
          val r = 1 + random.nextInt(numbers.length)
          at(r, numbers) match {
            case Some(value) if n > 0 => tailRecLotto(n - 1, removeAt(r, numbers), random, acc :+ value)
            case _ => acc
          }
      }
    }
    tailRecLotto(n, numbers, random, Nil)
  }

  def permutation[A](list: List[A], random: Random): List[A] = {
    @tailrec def tailRecPermutation(list: List[A], n: Int, random: Random, acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case _ if n > 0 =>
          val r = 1 + random.nextInt(n)
          at(r, list) match {
            case Some(value) if n > 0 => tailRecPermutation(removeAt(r, list), n - 1, random, acc :+ value)
            case _ => acc
          }
      }
    }
    tailRecPermutation(list, list.length, random, Nil)
  }


  def combinations[A](p: Int, list: List[A]): List[List[A]] = {
    def extractCombinations(p: Int, list: List[A], length: Int): List[List[A]] = {
      list match {
        case Nil => Nil
        case _ :: _ if p == length => List(list)
        case _ :: _ if p == 0 => List(Nil)
        case head :: tail => extractCombinations(p - 1, tail, length - 1).map(head :: _) ++ extractCombinations(p, tail, length - 1)
      }
    }

    extractCombinations(p, list, list.length)
  }

  def groups[A](p: Int, list: List[A]): List[List[A]] = {
    p match {
      case 0 => Nil
      case _ => groups(p - 1, list) ++ combinations(p, list)
    }
  }

  def specifiedGroups[A](groupSizes: List[Int], list: List[A]): List[List[A]] = {

    @tailrec def doSpecifiedGroups(groupSizes: List[Int], list: List[A], acc: List[List[A]] = Nil): List[List[A]] = {
      groupSizes match {
        case Nil => acc
        case 0 :: _ => acc
        case p :: moreSizes => doSpecifiedGroups(moreSizes, list, acc ++ combinations(p, list))
      }
    }
    doSpecifiedGroups(groupSizes.sorted.reverse, list)
  }


}
