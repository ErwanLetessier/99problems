package com.example._99problems

object Util {

  import java.text.NumberFormat
  import java.util.Locale

  val Formatter: NumberFormat = NumberFormat.getInstance(new Locale("en_US"))

  def time[R](block: => R, hint: String = ""): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"$hint: Elapsed time: ${Formatter.format(t1 - t0)} ns")
    result
  }
}
