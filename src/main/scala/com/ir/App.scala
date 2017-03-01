package com.ir

/**
 * @author ${user.name}
 */
object App {
  
  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)
  
  def main(args : Array[String]) {
    println( "Hello Wor Test1" +
      "vcxvxcld!" )
    println("concat  Test 2 arguments = " + foo(args))
  }

}
