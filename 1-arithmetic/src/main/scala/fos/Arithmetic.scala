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
      "true" ^^ { case e1 => {
	        println("True")
	        True()
	      }
      }
      | "false" ^^ { case e1 => {
    	  println("False")
    	  False()
      	} 
      }
      | "if" ~ Expr ~ "then" ~ Expr ~ "else" ~ Expr ^^ {case "if" ~ e1 ~ "then" ~ e2 ~ "else" ~ e3 => {
    	  println("If");
    	  If(e1, e2, e3)
      	}
      }
      | "0" ^^ { case e1 => {
    	  println("Zero");
    	  Zero()
      	} 
      }
      | "succ" ~> Expr ^^ { case e1 => {
    	  println("Succ");
    	  Succ(e1)
      	}
      }
      | "pred" ~> Expr ^^ { case e1 => {
    	  println("Pred");
    	  Pred(e1)
      	}
      }
      | "iszero" ~> Expr ^^ { case e1 => {
    	  println("Is zero")
    	  IsZero(e1)
      	}
      }
      | numericLit ^^ {case num => {
    	  println("NumericLit");
    	  decomposeNum(num.toInt)
      	}
      }
      | failure("illegal start of expression"))


  def decomposeNum(num: Int) : Term = {
    if (num <= 0) {
      println("Done")
      Zero()
    } else {
      println("In instance")
      Succ(decomposeNum(num - 1))
    }
  }
  
  def eval(t: Term):Term = {
    println(t);
    t match {
	    case If(e1, e2, e3) => {
	    		eval(e1) match {
	    		  case True() => eval(e2)
	    		  case False() => eval(e3)
	    		  case StuckTerm(err) => StuckTerm(err)
	    		  case err => StuckTerm(err)
	    		}
	    	}
	    case IsZero(e1) => {
	      eval(e1) match {
	        case Zero() => True()
	        case Succ(_) => False()
	        case StuckTerm(err) => StuckTerm(err)
	    	case err => StuckTerm(err)
	        }
	      }
	    case Pred(e1) => {
	      eval(e1) match {
	        case Zero() => Zero()
	        case Pred(e1) => Pred(Pred(e1))
	        case Succ(e2) => e2
	        case StuckTerm(err) => StuckTerm(err)
	    	case err => StuckTerm(err)
	      }
	    }
	    case Succ(e1) => {
	      eval(e1) match {
	        case Zero() => Succ(Zero())
	        case Succ(e1) => Succ(Succ(e1))
	        case Pred(e2) => e2
	        case StuckTerm(err) => StuckTerm(err)
	    	case err => StuckTerm(err)
	      }
	    }
	    case True() => True()
	    case False() => False()
	    case Zero() => Zero()
	    case err => err
	  }
  }
   
  def main(args: Array[String]): Unit = {
    println("Get system")
    var sys = System.in
    println("Get Input")
    var input = new java.io.InputStreamReader(sys)
    println("Get Token")
    
    var myData = "if true then true else false";
    val tokens = new lexical.Scanner(myData)
    //val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    println("Parse")
    phrase(Expr)(tokens) match {
      case Success(trees, _) => 
        println(trees);
        println("begin");
      	eval(trees)
        println("end");
      case e =>
        println(e)
    }
  }
}
