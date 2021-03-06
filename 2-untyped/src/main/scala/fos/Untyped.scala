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

   /** 
    *  Manage the parsing of the Applications 
    */
  def parseApplication(e2: List[Term]) : Term = e2.reverse match{
    case h1 :: Nil => {
      h1
    }
    case h1 :: t1 => {
    	Application(parseApplication(t1.reverse), h1)
    }
    case Nil => {
      throw NoRuleApplies(null)
    }
  }
    
  /** 
   *  Manage the names of the variables
   */
  def alpha(t: Term): FV = t match {
    case Variable(x) => FV(List(t.asInstanceOf[Variable]))
    case Abstraction(x, t) => alpha(t).remove(x)
    case Application(t1, t2) => {
      alpha(t1) union alpha(t2)
    }
    case Group(t1) => alpha(t1)
  }
  
  /** 
   *  Replace all character by a term 
   */
  def subst(t: Term, x: String, s: Term): Term = t match {
    case Variable(e1) => {
      if (e1 == x) {
        s
      } else {
        t
      }
    }
    case Abstraction(e1, t1) => {
      if(x==e1){
        Abstraction(e1,t1)
      }
      else{
      if (alpha(s) contains x) {
        subst(Abstraction(x+"1", subst(t1, x, Variable(x+"1"))), x, s)
      } else {
        Abstraction(e1, subst(t1, x, s))
      }
      }
    }
    case Application(t1, t2) => Application(subst(t1, x, s), subst(t2, x, s))
    case Group(t1) => Group(subst(t1, x, s))
  }

  /** Term 't' does not match any reduction rule. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** 
   *  Delete all the useless brackets 
   */
  def checkInner(t: Term): Term = t match {
    case Group(t1) => checkInner(t1)
    case _ => t
  }
  
  /** Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = {
    def inner(t: Term): Term = t match {
	    case Application(t1, t2) => checkInner(t1) match {
	      case Abstraction(e1, t3) => t1 match {
	        case Abstraction(e2, t4) => {
	          if (alpha(t2) contains e2) {
	        	  subst(t4, e2, subst(t2, e2, Variable(e2+"1")))
	          } else {
	        	  subst(t4, e2, t2) 
	          }
	        }
	        case Group(t4) => {
	          Group(inner(Application(checkInner(t4), t2)))
	        }
	      }
	      case _ => {
	        var s1 = inner(t1)
	        if (s1.toString() == t1.toString()){
	          Application(s1, inner(t2))
	        }
	        else Application(s1,t2)
	      }
	    }
	    case Abstraction(e1, t1) => {
	      Abstraction(e1, inner(t1))
	    }
	    case Group(t1) => checkInner(inner(t1)) match {
	      case t2: Variable => {
			    	  t2
			    }
	      case t2 => t2
	    }
	    case _ => {
	    	 t
	    }
    }
    
    var temp = inner(t);
    
    if (temp.toString() == t.toString()) {
      throw NoRuleApplies(t);
    } else {
      temp
    }
  }
  
  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = {
    def inner(t: Term): Term = t match {
	    case Application(t1, t2) => {
	      checkInner(t2) match {
		      case a1: Abstraction =>  {
		        checkInner(t2) match {
		          case a2: Abstraction => {
			        checkInner(t1) match {
					      case Abstraction(e1, t4) => {
					        if (alpha(a1) contains e1) {
					        	subst(t4, e1, subst(a1, e1, Variable(e1+"1")))
					        } else {
					        	subst(t4, e1, a1) 
					        }
					      }
					      case _ => {
					        Application(inner(t1), inner(t2))
					      }
				      }

		          }
		          case _ => Application(t1, inner(t2))
		        }
		      }
		      case _ => {
	        var s1 = inner(t1)
	        if (s1.toString() == t1.toString()){
	          Application(s1, inner(t2))
	        }
	        else Application(s1,t2)
	      }
	      }
	    }
	    case Abstraction(e1, t1) => Abstraction(e1, inner(t1))
	    case Group(t1) => checkInner(inner(t1)) match {
	      case e: Variable => e
	      case t2 => Group(t2)
	    }
	    case _ => {
	      t
	    }
	  }
    
    var temp = inner(t);
    
    if (temp.toString() == t.toString()) {
      throw NoRuleApplies(t);
    } else {
      temp
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
    //*
		  var myData = "a b c d e f g h i j k";
     //var myData = "((\\y. y) \\y.y)";
     val tokens = new lexical.Scanner(myData)
     System.out.println("----------------------------------------------------------");
     //val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
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
