package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the
 *  simply typed lambda calculus found in Chapter 9 of
 *  the TAPL book.
 */
object SimplyTyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*")
  lexical.reserved   ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
                              "pred", "iszero", "let", "in", "fst", "snd")

  /** Term     ::= SimpleTerm { SimpleTerm }
   */
  def Term: Parser[Term] = positioned(
	rep1(SimpleTerm) ^^ {
		case e1 => {
			parseApplication(e1)
		}
	}
    | failure("illegal start of term"))

  /** SimpleTerm ::= "true"
   *               | "false"
   *               | number
   *               | "succ" Term
   *               | "pred" Term
   *               | "iszero" Term
   *               | "if" Term "then" Term "else" Term
   *               | ident
   *               | "\" ident ":" Type "." Term
   *               | "(" Term ")"
   *               | "let" ident ":" Type "=" Term "in" Term
   *               | "{" Term "," Term "}"
   *               | "fst" Term
   *               | "snd" Term
   */
  def SimpleTerm: Parser[Term] = positioned(
      "true"          ^^^ True
    | "false"         ^^^ False
    | "0" ^^^ Zero
      | "if" ~ Term ~ "then" ~ Term ~ "else" ~ Term ^^ {case "if" ~ e1 ~ "then" ~ e2 ~ "else" ~ e3 => {
    	  If(e1, e2, e3)
      	}
      }
      | "succ" ~> Term ^^ { case e1 => {
    	  Succ(e1)
      	}
      }
      | "pred" ~> Term ^^ { case e1 => {
    	  Pred(e1)
      	}
      }
      | "iszero" ~> Term ^^ { case e1 => {
    	  IsZero(e1)
      	}
      }
      | numericLit ^^ {case num => {
    	  decomposeNum(num.toInt)
      	}
      }
    |ident ^^ {case str => {
			Variable(str)
		}
	}
	| ("\\" ~> ident) ~ (":" ~> Type) ~ ("." ~> Term) ^^ { case str ~ t1 ~ e1 => {
			Abstraction(str,t1, e1)
		}
	}
	| "(" ~> Term <~ ")" ^^ { case e1 => {
			Group(e1)
		}
	}
	| ("let" ~> ident) ~ (":" ~> Type) ~ ("=" ~> Term) ~ ("in" ~> Term)^^ { case str ~ t1 ~ e1 ~ e2 => {
			Let(str,t1, e1,e2)
		}
	}
	| "{" ~> Term ~","~ Term <~ "}" ^^ { case e1 ~","~ e2 => {
			Pair(e1,e2)
		}
	}
	| "fst" ~> Term ^^ { case e1 => {
			First(e1)
		}
	}
	| "snd" ~> Term ^^ { case e1 => {
			Second(e1)
		}
	}
    | failure("illegal start of simple term"))

  /** Type       ::= SimpleType [ "->" Type ]
   */
  def Type: Parser[Type] = positioned(
 failure("illegal start of type"))

  //   ... To complete ... 
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
  
  def parseType(e2: List[Type]) : Term = e2 match{
    case h1 :: Nil => {
      h1
    }
    case h1 :: t1 => {
    	FunctionType(h1, parseApplication(t1))
    }
    case Nil => {
      throw NoRuleApplies(null)
    }
  }
    
   def decomposeNum(num: Int) : Term = {
    if (num <= 0) {
      Zero
    } else {
      NumericSucc(decomposeNum(num - 1))
    }
  }
  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(pos: Position, msg: String) extends Exception(msg) {
    override def toString =
      msg + "\n" + pos.longString
  }

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, Type)]

  /** Is the given term a numeric value? */
  def isNumericVal(t: Term): Boolean = t match {
  //   ... To complete ... 
    case _ => false
  }

  /** Is the given term a value? */
  def isValue(t: Term): Boolean = t match {
  //   ... To complete ... 
    case _ => false
  }

  /** Call by value reducer. */
  def reduce(t: Term): Term = t match {
  //   ... To complete ... 
    case _ =>
      throw NoRuleApplies(t)
  }

  /** Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context, t: Term): Type = t match {
    case True | False =>
      TypeBool
  //   ... To complete ... 
  }

  /** Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the evaluation strategy used for reduction.
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
        try {
          println("typed: " + typeof(Nil, trees))
          for (t <- path(trees, reduce))
            println(t)
        } catch {
          case tperror => println(tperror.toString)
        }
      case e =>
        println(e)
    }
  }
}
