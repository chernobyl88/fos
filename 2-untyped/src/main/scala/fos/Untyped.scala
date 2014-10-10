package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the
 *  untyped lambda calculus found in Chapter 5 of
 *  the TAPL book.
 */
object Untyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".")
  import lexical.Identifier

  
  def Elem: Parser[Term] = (
     ident ^^ {case str => {
			Variable(str)
		}
	}
	| ("\\" ~> ident) ~ ("." ~> Term) ^^ { case str ~ e1 => {
			Abstraction(str, e1)
		}
	}
	| "(" ~> Term <~ ")" ^^ { case e1 => {
			Group(e1)
		}
	}
  )
  
  def Term: Parser[Term] = (
	rep1(Elem) ^^ {
		case e1 => {
			parseApplication(e1)
		}
	}
    | failure("illegal start of term"))

  def parseApplication(e2: List[Term]) : Term = e2 match{
    case h1 :: h2 :: Nil => {
    	Application(h1, h2)
    }
    case h1 :: h2 :: t1 => {
    	Application(Application(h1, h2), parseApplication(t1))
    }
    case h1 :: Nil => {
      h1
    }
    case Nil => {
      throw NoRuleApplies(null)
    }
  }
    
  def alpha(t: Term): FV = t match {
    case Variable(x) => FV(List(t.asInstanceOf[Variable]))
    case Abstraction(x, t) => alpha(t).remove(x)
    case Application(t1, t2) => alpha(t1) union alpha(t2)
    case Group(t1) => alpha(t1)
  }
  
  def subst(t: Term, x: String, s: Term): Term = t match {
    case Variable(e1) => {
      if (e1 == x) {
        s
      } else {
        t
      }
    }
    case Abstraction(e1, t1) => {
      if (e1 == x) {
        subst(Abstraction(x+"1", subst(t1, x, Variable(x+"1"))), x, s)
      } else {
        Abstraction(e1, subst(t1, x, s))
      }
    }
    case Application(t1, t2) => Application(subst(t1, x, s), subst(t2, x, s))
    case Group(t1) => Group(subst(t1, x, s))
  }

  /** Term 't' does not match any reduction rule. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  def checkInner(t: Term): Term = t match {
    case Group(t1) => checkInner(t1)
    case _ => t
  }
  
  /** Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = t match {
	    case Application(t1, t2) => checkInner(t1) match {
	      case Abstraction(e1, t3) => t1 match {
	        case Abstraction(e2, t4) => {
	          subst(t4, e2, t2)
	        }
	        case Group(Abstraction(e2, t4)) => {
	          subst(t4, e2, t2)
	        }
	        case Group(t4) => {
	          println(Application(checkInner(t4), t2))
	          Group(reduceNormalOrder(Application(checkInner(t4), t2)))
	        }
	      }
	      case _ => Application(reduceNormalOrder(t1), reduceNormalOrder(t2)) // <- s'arrête sur la première variable
	    }
	    case Abstraction(e1, t1) => {
	      Abstraction(e1, reduceNormalOrder(t1))
	    }
	    case Group(t1) => checkInner(reduceNormalOrder(t1)) match {
	      case t2: Variable => {
			    	  throw NoRuleApplies(t2)
			    }
	      case t2 => t2
	    }
	    case _ => {
	    	 throw NoRuleApplies(t)
	    }
  }
  
  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = t match {
    case Application(t1, t2) => {
      checkInner(reduceCallByValue(t2)) match { // <- ici, la récurssion s'arrête, car arrive sur une variable et donc une exception...
	      case Abstraction(e2, t3) =>  {
	        checkInner(t1) match {
			      case Abstraction(e1, t4) => {
			        subst(Abstraction(e1, t4), e1, Abstraction(e2, t3))
			      }
			      case _ => {
			        Application(reduceCallByValue(t1), reduceCallByValue(t2))
			      }
		      }
	      }
	      case _ => {
	        Application(reduceCallByValue(t1), reduceCallByValue(t2))
	      }
      }
    }
    case Abstraction(e1, t1) => Abstraction(e1, reduceCallByValue(t1)) // Est conscidéré comme une valeur. Doit-il être traité quand même?
    case Group(t1) => checkInner(reduceCallByValue(t1)) match {
      case e: Variable => throw NoRuleApplies(e) 
      case t2 => Group(t2)
    }
    case _ => {
      throw NoRuleApplies(t) // Cause de l'exception
    }
  }

  /** Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the method that reduces a term by one step.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): Stream[Term] = 
    try {
      var t1 = reduce(t)
      Stream.cons(t, path(t1, reduce))
    } catch {
      case NoRuleApplies(_) =>
        Stream.cons(t, Stream.empty)
    }


  def main(args: Array[String]): Unit = {
    
    //var myData = "((\\x. x) y) (\\x. ((((\\y. y)))) ((\\z. z) ((\\ w. w) \\t. t)))";
    var myData = "((\\x. x) y)";
    val tokens = new lexical.Scanner(myData)
    System.out.println("----------------------------------------------------------");
   // val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    phrase(Term)(tokens) match {
      case Success(trees, _) =>
        println("normal order: ")
        for (t <- path(trees, reduceNormalOrder))
          println(t)
        println("call-by-value: ")
        for (t <- path(trees, reduceCallByValue))
          println(t)

      case e =>
        println(e)
    }    
  }
}
