package example

import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {
    // for example (+ 1 2)
    println("Enter smth (for example, (+ 1 2)) :")
    val input: String = readLine()
    val ans = Parser.apply(input)
    println(ans)
  }

}
