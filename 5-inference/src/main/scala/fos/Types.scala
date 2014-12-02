package fos

import scala.collection.immutable.{Set, ListSet}

abstract class Type {
  override def toString() = this match {
    case TypeVar(a) => a
    case TypeFun(a, b) => "(" + a + " -> " + b + ")"
    case TypeNat => "Nat"
    case TypeBool => "Bool"
  }
}
  case class TypeError(msg: String) extends Exception(msg)

case class TypeVar(name: String) extends Type
  //   ... To complete ... 
case class TypeFun(a: Type, b:Type) extends Type
  //   ... To complete ... 
case object TypeNat extends Type
  //   ... To complete ... 
case object TypeBool extends Type
  //   ... To complete ... 

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
  def instantiate : Type = {
    def chaining(t: Type, tv: TypeVar, n: TypeVar): Type = t match {
      case x: TypeVar if (x == tv) => tv
      case TypeFun(a, b) => TypeFun(chaining(a, tv, n), chaining(b, tv, n))
      case _ => t
    }
    (tp /: args)(chaining(_, _, FreshName.newName()))
  }
  override def toString() = args.mkString("[", ", ", "].") + tp
}

object Type {
  //   ... To complete ... 
}


object FreshName {
  var nbr = 0
  def newName(): TypeVar = {
    nbr += 1
    new TypeVar("_" + nbr)
  }
}

abstract class Substitution extends (Type => Type) {

  var indent = 0
  
  def lookup(t: TypeVar) : Type;

  //   ... To complete ... 
  def apply(tp: Type): Type = {
    
    //checkSanity();
    
    //println("  " * indent + "in: " + tp + "   subst: " + this)
    indent = indent + 1
    val result = tp match {
	    case x: TypeVar => lookup(x)
	    case TypeFun(a, b) => TypeFun(apply(a), apply(b)) 
	    case _ => tp
    }
    indent = indent - 1
    //println("  " * indent + "out: " + result + "   subst: " + this)
    result
  }
  override def toString() = ""

  def apply(p: (Type, Type)): (Type, Type) = p match {
    case Pair(t1, t2) => (this(t1), this(t2))
  }
  
  def checkSanity(): Substitution

  def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] = {
    env map {
      (pair) => (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp)))
      }
    
  }

  def extending(t: TypeVar, T: Type): Substitution
}

class oneSubst(constr : List[(TypeVar, Type)]) extends Substitution {
  
  def lookup(t: TypeVar, c: List[(TypeVar, Type)]): Type = { //problem could be here.... shoold use checksanity?
	def inner (t: TypeVar, r: List[(TypeVar, Type)], f: List[(TypeVar, Type)]): List[(TypeVar, Type)] = r match {
    	case Nil => f	
    	case (x:TypeVar, y:Type) :: tail => y match {
    	  case a: TypeVar => inner(t, tail, (t, lookup(a)) :: f)
    	  case TypeFun(a:TypeVar, b:TypeVar) => inner(t, tail, (t, TypeFun(lookup(a), lookup(b))) :: f)
    	  case TypeFun(a:TypeVar, b) => inner(t, tail, (t, TypeFun(lookup(a), b)) :: f)
    	  case TypeFun(a, b:TypeVar) => inner(t, tail, (t, TypeFun(a, lookup(b))) :: f)
    	  case a => inner(t, tail, (t, a) :: f)
    	}
	}
	def checker(t: TypeVar, c: List[(TypeVar, Type)]): Type = c match {
	  case Nil => t
	  case (_, b) :: Nil => b
	  case (_, b) :: tail => if (b == checker(t, tail)) b else throw TypeError("TypeError on: " + t + " typed as "+ b +" and as " + checker(t, tail) + "")
	}

   	checker(t, inner(t, c.filter(ti => ti._1 == t), List()))
  }
  
  override def lookup(t: TypeVar) : Type = {
   lookup(t, constr)
  }
  
  override def toString() = {
    def inner(c: List[(TypeVar, Type)]) : String  = c match{
      case (a, b) :: Nil => "(" + a + " -> " + b + ")"
      case (a, b) :: tail => "(" + a + " -> " + b + ") ; " + inner(tail)
    }
    inner(constr)
  }
  
  def checkSanity() : Substitution = {
	def chaining (a: TypeVar, f: Type, list: List[(TypeVar, Type)]): List[(TypeVar, Type)] = list match {
		case Nil => List()
		case (t: TypeVar, u: TypeVar) :: tail if (f == u) => chaining(a, f, tail)
		case (t: TypeVar, u: TypeVar) :: tail => (u, f) :: chaining(a, f, tail)
		case (_, x) :: tail if (x == f)=> (a, f) :: chaining(a, f, tail)
		case (_, x) :: tail => throw TypeError("TypeError on: " + a + " typed as "+ f +" and as " + x + "")
	}
    
    def checker(b: Type, cons: List[(TypeVar, Type)], list: List[(TypeVar, Type)]) : List[(TypeVar, Type)] = list match {
      case Nil => cons
      case (a, c: TypeVar) :: tail if (cons.filter(i => i._1 == a && i._2 == c).size > 0) => checker (b, cons, tail)
	  case (a, c:TypeVar) :: tail => checker(b, (a, c) :: cons, tail)
	  //case (a, TypeFun(c1, c2)) :: tail => (a, TypeFun(c1, c2)) :: checker(b, cons, tail) TODO
	  case (a, c) :: tail => b match {
	    case x: TypeVar => checker(b, (a, c) :: (x, c) :: cons, tail)
	    case _ => {
	      if (cons.filter(i => i._1 == a && i._2 != c).size == 0)
	        checker(b, (a, c) :: cons, tail)
	      else {
	    	  checker(b, chaining(a, b, cons.filter(i => i._1 == a)) ::: cons.filter(i => i._1 != a), tail)
	      }
	    }
	  }
    }
    
    def inner (cons: List[(TypeVar, Type)], list: List[(TypeVar, Type)]) : Substitution = cons match {
      case (a, b) :: tail => {
        inner(tail, (checker(b, list, (a, b) :: list.filter(i => i._1 == a)) ::: list).distinct)
      }
      case Nil => {
        new oneSubst(list)
      }
    }
    inner(constr, List())
  }
  
  override def extending(x:TypeVar, y:Type):Substitution = new oneSubst((x, y) :: constr)
}

class CoupleSubst(s1: Substitution, s2: Substitution) extends Substitution {
  var cons1 = s1
  var cons2 = s2
  override def lookup(t: TypeVar) : Type = {
    val t1 = cons1.lookup(t)
    if (t1 == t) cons2.lookup(t) else t1
  }
  
  override def apply(tp: Type) = cons2(cons1(tp))
  override def apply(tp: (Type, Type)) = cons2(cons1(tp))
  override def extending(t: TypeVar, T: Type) = cons2.extending(t: TypeVar, T: Type)
  override def checkSanity():Substitution = new CoupleSubst(s1.checkSanity, s2.checkSanity)
}

/** The empty substitution. */
object emptySubst extends Substitution {
  def lookup(t: TypeVar) = t
  override def extending(x:TypeVar, y:Type):Substitution = new oneSubst((x, y) :: Nil)
  override def checkSanity():Substitution = emptySubst
}
