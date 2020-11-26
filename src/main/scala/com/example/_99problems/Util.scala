package com.example._99problems

object Util {

  def time[R](block: => R, hint: String = ""): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"$hint: Elapsed time: ${t1 - t0} ns")
    result
  }
}
