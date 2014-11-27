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
  //   ... To complete ... 
  override def toString() = args.mkString("[", ", ", "].") + tp
}

object Type {
  //   ... To complete ... 
}

abstract class Substitution extends (Type => Type) {

  var indent = 0
  var constr : List[(TypeVar, Type)] = Nil;
  
  def lookup(t: TypeVar) : Type;

  //   ... To complete ... 
  def apply(tp: Type): Type = {
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

  def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] =
    env map { (pair) => (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp))) }

  def extending(t: TypeVar, T: Type) = constr = (t, T) :: constr
}

class oneSubst extends Substitution {
  
  def lookup(t: TypeVar, c: List[(TypeVar, Type)]): Type = c match {
    case Nil => t
    case hCons :: tCons => if (hCons._1 == t) hCons._1 else lookup(t, tCons)
  }
  
  override def lookup(t: TypeVar) : Type = {
   lookup(t, constr)
  }
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
}

/** The empty substitution. */
object emptySubst extends Substitution {
  def lookup(t: TypeVar) = t
}
