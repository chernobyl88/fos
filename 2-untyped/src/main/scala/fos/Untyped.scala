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

  /** Term     ::= AbsOrVar { AbsOrVar }
   */
  def Term: Parser[Term] = (
	stringLit ^^ {case e1 => {
			Variable(e1)
		}
	}
	| "\\" ~ stringLit ~ "." ~ Term ^^ { case "\\" ~ e1 ~ "." ~ e2 => {
			Abstraction(e1, e2)
		}
	}
	| Term ~ Term ^^ { case e1 ~ e2 => {
			Application(e1, e2)
		}
	}
	| "(" ~ Term ~ ")" ^^ { case "(" ~ e1 ~ "~" => {
			Group(e1)
		}
	}
    | failure("illegal start of term"))
    
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

  /** Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = t match {
    case Variable(e1) => t
    case Application(t1, t2) => t1 match {
      case Abstraction(e1, t1) => reduceNormalOrder(subst(t1, e1, t2))
      case _ => Application(reduceNormalOrder(t1), reduceNormalOrder(t2))
    }
    case Abstraction(e1, t1) => Abstraction(e1, reduceNormalOrder(t1))  // Boucle infinie? Si t1 ireductible rŽaliser l abstraction
    case Group(t1) => Group(reduceNormalOrder(t1)) // Boucle infinie? Si t1 ireductible enlever les parentheses
  }

  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = t match {
  //   ... To complete ... 
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
    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
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
