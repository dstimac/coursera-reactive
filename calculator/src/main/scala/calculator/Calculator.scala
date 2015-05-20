package calculator

import scala.annotation.tailrec

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    def isCircular(name: String, expr: Expr): Boolean = {
      expr match {
        case x : Literal   => false
        case Plus(x, y)    => isCircular(name, x) || isCircular(name , y)
        case Minus(x, y)   => isCircular(name, x) || isCircular(name , y)
        case Times(x, y)   => isCircular(name, x) || isCircular(name , y)
        case Divide(x, y)  => isCircular(name, x) || isCircular(name , y)
        case Ref(n)        => n == name || isCircular(name, getReferenceExpr(n, namedExpressions))
      }
    }

    namedExpressions.map{ e => e._1 -> Signal{
      if(isCircular(e._1, e._2())) {
        Double.NaN
      } else eval(e._2(), namedExpressions)
    }}
  }


  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(x)   => x
      case Plus(x, y)   => eval(x, references) + eval(y, references)
      case Minus(x, y)  => eval(x, references) - eval(y, references)
      case Times(x, y)  => eval(x, references) * eval(y, references)
      case Divide(x, y) => eval(x, references) / eval(y, references)
      case Ref(n) =>
        getReferenceExpr(n, references) match {
          case Literal(x) => x
          case ex: Expr   => eval(ex, references)
        }
    }
  }

//  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
//    namedExpressions.map{ e => e._1 -> Signal(eval(e._2(), namedExpressions))}
//  }
//
//  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
//
//    def checkRecursiveSignal(callerName: String, expr: Expr): Expr ={
//      expr match {
//        case x : Literal   => x
//        case Plus(x, y)    => Plus(checkRecursiveSignal(callerName, x), checkRecursiveSignal(callerName, y))
//        case Minus(x, y)   => Minus(checkRecursiveSignal(callerName, x), checkRecursiveSignal(callerName, y))
//        case Times(x, y)   => Times(checkRecursiveSignal(callerName, x), checkRecursiveSignal(callerName, y))
//        case Divide(x, y)  => Divide(checkRecursiveSignal(callerName, x), checkRecursiveSignal(callerName, y))
//        case Ref(n)        =>
//          if(callerName == n) Literal(Double.NaN)
//          else {
//            getReferenceExpr(n, references) match {
//              case Literal(x) => Literal(x)
//              case ex: Expr   => checkRecursiveSignal(callerName, ex)
//            }
//          }
//      }
//    }
//
//    expr match {
//      case Literal(x)   => x
//      case Plus(x, y)   => eval(x, references) + eval(y, references)
//      case Minus(x, y)  => eval(x, references) - eval(y, references)
//      case Times(x, y)  => eval(x, references) * eval(y, references)
//      case Divide(x, y) => eval(x, references) / eval(y, references)
//      case Ref(n) =>
//        getReferenceExpr(n, references) match {
//          case Literal(x) => x
//          case ex: Expr   => eval(checkRecursiveSignal(n, ex), references)
//        }
//    }
//  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
