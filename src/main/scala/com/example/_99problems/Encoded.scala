package com.example._99problems

sealed trait Encoded[A]

case class One[A](value: A) extends Encoded[A]
case class Many[A](times: Int, value: A) extends Encoded[A]
