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
	        True()
	      }
      }
      | "false" ^^ { case e1 => {
    	  False()
      	} 
      }
      | "if" ~ Expr ~ "then" ~ Expr ~ "else" ~ Expr ^^ {case "if" ~ e1 ~ "then" ~ e2 ~ "else" ~ e3 => {
    	  If(e1, e2, e3)
      	}
      }
      | "0" ^^ { case e1 => {
    	  Zero()
      	} 
      }
      | "succ" ~> Expr ^^ { case e1 => {
    	  Succ(e1)
      	}
      }
      | "pred" ~> Expr ^^ { case e1 => {
    	  Pred(e1)
      	}
      }
      | "iszero" ~> Expr ^^ { case e1 => {
    	  IsZero(e1)
      	}
      }
      | numericLit ^^ {case num => {
    	  decomposeNum(num.toInt)
      	}
      }
      | failure("illegal start of expression"))


  def decomposeNum(num: Int) : Term = {
    if (num <= 0) {
      Zero()
    } else {
      NumericSucc(decomposeNum(num - 1))
    }
  }
  
  def eval(t: Term):Term = {
    t match {
	    case If(e1, e2, e3) => {
	    		eval(e1) match {
	    		  case True() => eval(e2)
	    		  case False() => eval(e3)
	    		  case StuckTerm(err) => e1
	    		  case err => StuckTerm(t)
	    		}
	    	}
	    case IsZero(e1) => {
	      eval(e1) match {
	        case Zero() => True()
	        case Succ(_) => False()
	        case StuckTerm(err) => StuckTerm(err)
	    	case err => StuckTerm(t)
	        }
	      }
	    case Pred(e1) => {
	      eval(e1) match {
	        case Zero() => Zero()
	        case Pred(e1) => Pred(Pred(e1))
	        case Succ(e2) => e2
	        case StuckTerm(err) => StuckTerm(err)
	    	case err => StuckTerm(t)
	      }
	    }
	    case Succ(e1) => {
	      eval(e1) match {
	        case Zero() => Succ(Zero())
	        case Succ(e1) => Succ(Succ(e1))
	        case Pred(e2) => e2
	        case StuckTerm(err) => StuckTerm(err)
	    	case err => StuckTerm(t)
	      }
	    }
	    case True() => True()
	    case False() => False()
	    case Zero() => Zero()
	    case err => err
	  }
  }
  
  def reduction(t: Term): Term = {
    t match {
      case If (e1, e2, e3) => {
        e1 match {
          case True() => e2
          case False() => e3
          case term:Value => StuckTerm(t)
          case _ => If(reduction(e1), e2, e3)
        }
      }
      case IsZero(e1) => {
        e1 match {
          case Zero() => True()
          case Succ(num:Numeric) => False()
          case term:Value => StuckTerm(t)
          case _ => IsZero(reduction(e1))
        }
      }
      case Pred(e1) => {
        e1 match {
	        case Zero() => Zero()
	        case Succ(num:Numeric) => num
	        case term:Value => StuckTerm(t)
	        case _ => Pred(reduction(e1))
        }
      }
      case Succ(e1) => {
        e1 match {
          case num:Numeric => NumericSucc(num)
          case term:Value => StuckTerm(t)
          case _ => Succ(reduction(e1))
        }
      }
      case _ => t
    }
  }
   
  def main(args: Array[String]): Unit = {
    
    var myData = "if 1 then true else false";
   // myData = "pred succ succ succ false"
    val tokens = new lexical.Scanner(myData)
   // val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    phrase(Expr)(tokens) match {
      case Success(trees, _) =>
        
        var currTerm = trees
        var finished = false
        
        while (!finished) {
          currTerm match {
            case term:Value => {
              currTerm match {
                case StuckTerm(t) => println("Stuck term: " + term)
                case term => println(term)
              }
              finished = true
            }
            case term => {
          println(currTerm)
              currTerm = reduction(term)
            }
          }
        }
        
        print("Big Step: ");
      	eval(trees) match {
      	  case StuckTerm(e) => println("Stuck term: " + e)
      	  case term => println(term)
      	}
      case e =>
        println(e)
    }
  }
}
