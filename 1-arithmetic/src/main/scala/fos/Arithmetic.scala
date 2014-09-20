package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit
  
  /** Expr ::= 'true'
      | 'false'
      | 'if' Expr 'then' Expr 'else' Expr
      | '0'
      | 'succ' Expr
      | 'pred' Expr
      | 'iszero' Expr
  */
  def Expr: Parser[Term] = (
      "true" ~> Expr ^^ { case e1 => True() }
      | "false" ~> Expr ^^ { case e1 => False() }
      | "if" ~> Expr ~ "then" ~> Expr ~ "else" ~> Expr
      | "0" ~> Expr ^^ { case e1 => Zero() }
      | "succ" ~> Expr ^^ { case e1 => Succ(e1)}
      | "pred" ~> Expr ^^ { case e1 => Pred(e1) }
      | "iszero" ~> Expr ^^ { case e1 => IsZero(e1) }
      | failure("illegal start of expression"))


  //   ... To complete ... 
   
  def main(args: Array[String]): Unit = {
    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    phrase(Expr)(tokens) match {
      case Success(trees, _) => 
      	trees match {
      	  case Succ(e1) => e1 match {
      	    	case Pred(e1) => println(e1)
      	    	case e1 => println("Stuck term: Succ(" + e1 + ")")
      	  }
      	  case Pred(e1) => e1 match {
      	    	case Succ(e1) => println(e1)
      	    	case Zero() => println("0")
      	    	case e1 => println("Stuck term: Succ(" + e1 + ")")
      	  }
      	  case IsZero(e1) => e1 match {
      	    	case Zero() => println("true")
      	    	case e1 => println("false")
      	  }
      	  case True() => println("true")
      	  case False() => println("false")
      	}
      case e =>
        println(e)
    }
  }
}
