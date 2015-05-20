package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  def solve(a: Double, b: Double, c: Double): Signal[Set[Double]] = {
    val aa = Signal(a)
    val bb = Signal(b)
    val cc = Signal(c)
    Polynomial.computeSolutions(aa, bb, cc, Polynomial.computeDelta(aa, bb, cc))
  }
  test("simple Polynomials") {
    // x^2 -> a = 1
    val res  = solve(1, 0, 0)
    // x   -> b = 1
    val res1 = solve(0, 1, 0)
    // 1   -> c = 1
    val res2 = solve(0, 0, 1)

    assert(res().size == 1 && res().head == 0)
    assert(res1().map{ e =>
      e.equals(Double.NaN) || e.equals(Double.NegativeInfinity)
    }.forall(_ == true))
    assert(res2().forall(_.equals(Double.NaN)))
  }

  test("x^2 - 2x") {
    val res = solve(1, -2, 0)
    assert(res().size == 2 && res().contains(2.0) && res().contains(0))
  }

  test("2x^2 + x + 1") {
    val res = solve(2, 1, 1)
    assert(res().size == 0)
  }


  test("calculator with recursive name call") {
    val map: Map[String, Signal[Expr]] =  Map("a" -> Signal(Literal(1)), "b" -> Signal(Plus(Ref("b"), Literal(1))))
    val res = Calculator.computeValues(map)
    assert(res("a")() == 1 && res("b")().equals(Double.NaN))
  }

  test("calculator with for simple functions") {
    val map: Map[String, Signal[Expr]] =  Map("a" -> Signal(Literal(1)), "b" -> Signal(Plus(Literal(1), Literal(1))))
    val res = Calculator.computeValues(map)
    assert(res("a")() == 1 && res("b")() == 2)

    val map1: Map[String, Signal[Expr]] =  Map("a" -> Signal(Literal(1)), "b" -> Signal(Minus(Literal(1), Literal(1))))
    val res1 = Calculator.computeValues(map1)
    assert(res1("a")() == 1 && res1("b")() == 0)

    val map2: Map[String, Signal[Expr]] =  Map("a" -> Signal(Literal(1)), "b" -> Signal(Divide(Literal(4), Literal(2))))
    val res2 = Calculator.computeValues(map2)
    assert(res2("a")() == 1 && res2("b")() == 2)

    val map3: Map[String, Signal[Expr]] =  Map("a" -> Signal(Literal(1)), "b" -> Signal(Times(Literal(4), Literal(2))))
    val res3 = Calculator.computeValues(map3)
    assert(res3("a")() == 1 && res3("b")() == 8)
  }

  test("calculator unknown reference"){
    val map: Map[String, Signal[Expr]] =  Map("a" -> Signal(Ref("b")))
    val res = Calculator.computeValues(map)
    assert(res("a")().equals(Double.NaN))
  }

  test("calculator long chain")  {
    val map: Map[String, Signal[Expr]] =  Map(
        "a" -> Signal(Ref("b"))
      , "b" -> Signal(Ref("c"))
      , "c" -> Signal(Ref("d"))
      , "d" -> Signal(Ref("e"))
      , "e" -> Signal(Ref("f"))
      , "f" -> Signal(Literal(1))
    )
    val res = Calculator.computeValues(map)
    assert(List(res("a")(),res("b")(), res("c")(), res("d")(), res("e")(), res("f")()).sum == 6)
  }

  test("calculator long unbroken chain")  {
    val map: Map[String, Signal[Expr]] =  Map(
        "a" -> Signal(Ref("b"))
      , "b" -> Signal(Ref("c"))
      , "c" -> Signal(Ref("d"))
      , "d" -> Signal(Ref("e"))
      , "e" -> Signal(Ref("f"))
      , "f" -> Signal(Ref("a"))
    )
    val res = Calculator.computeValues(map)
    assert(res("a")().equals(Double.NaN))
    assert(res("b")().equals(Double.NaN))
    assert(res("c")().equals(Double.NaN))
    assert(res("d")().equals(Double.NaN))
    assert(res("e")().equals(Double.NaN))
    assert(res("f")().equals(Double.NaN))
  }
}
