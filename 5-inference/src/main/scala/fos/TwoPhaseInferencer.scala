package fos

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {
  import Type._

  type Constraint = (Type, Type)

  val noConstraints: List[Constraint] = Nil
  case class TypingResult(tpe: Type, c: List[Constraint])

  /** Type <code>t</code> in <code>env</code> and return its type and a
   *  constraint list.
   */
  def collect(env: Env, t: Term): TypingResult = t match {
    case Var(x) =>
      val t1 = lookup(env, x)
      if (t1 == null)
        throw TypeError("Unknown variable " + x)
      TypingResult(t1.instantiate, noConstraints)
    case True() => TypingResult(TypeBool, noConstraints)
    case False() =>  TypingResult(TypeBool, noConstraints)
    case Zero() => TypingResult(TypeNat, noConstraints)
    case Succ(t) => {
      val TypingResult(pT, c) = collect(env, t)
      TypingResult(TypeNat, (pT, TypeNat) :: c)
    }
    case Pred(t) => {
      val TypingResult(pT, c) = collect(env, t)
      TypingResult(TypeNat, (pT, TypeNat) :: c)
    }
    case IsZero(t) => {
      val TypingResult(pT, c) = collect(env, t)
      TypingResult(TypeNat, (pT, TypeNat) :: c)
    }
    case If(t1, t2, t3) => {
      val TypingResult(pT1, c1) = collect(env, t1)
      val TypingResult(pT2, c2) = collect(env, t2)
      val TypingResult(pT3, c3) = collect(env, t3)
      TypingResult(pT2, (pT1, TypeBool) :: (pT2, pT3) :: c1 ::: c2 ::: c3)
    }
    case Abs(v, tp, t) => {
      tp match {
        case EmptyType => {
          val freshVar = FreshName.newName
          val TypingResult(pT, c) = collect((v, TypeScheme(List(freshVar), freshVar)) :: env, t)
          TypingResult(TypeFun(freshVar,pT), c)
        }
        case _ => {
	      val TypingResult(pT, c) = collect((v, TypeScheme(List(), tp.toType)) :: env, t)
	      TypingResult(TypeFun(tp.toType, pT), c)
        }
      }
    }
    case App(t1, t2) => {
      val freshVar = FreshName.newName
      val TypingResult(pT1, c1) = collect(env, t1)
      val TypingResult(pT2, c2) = collect(env, t2)
      TypingResult(freshVar, (pT1, TypeFun(pT2, freshVar)) :: c1 ::: c2)
    }
    case Let(x, v, t) => {
      val freshVar = FreshName.newName
      val TypingResult(pTv, cv) = collect(env, v)
      
      val subst = unify(cv)
      val S = subst(pTv)
      
      val TypingResult(pT, c) = collect((x, TypeScheme(isFree(subst(env), freshVar), freshVar)) :: env, t)
      TypingResult(TypeFun(S, pT), c)
    }
  }
  
  def isFree(env: Env, t: Type): List[TypeVar] = t match {
    case TypeVar(x) if lookup(env, x) != Nil => TypeVar(x) :: Nil
    case TypeFun(a, b) => isFree(env, a) ::: isFree(env, b)
    case _ => Nil
  } 

  /**
   */
  def unify(c: List[Constraint]): Substitution =
    if (c.isEmpty) emptySubst
    else c.head match {
      case (TypeVar(a), TypeVar(b)) if (a == b) =>
        unify(c.tail)
      case (TypeVar(a), t) =>{
        var d = new oneSubst()
        d.extending(TypeVar(a), t)
        new CoupleSubst(d,unify(c.tail))
      }
      case (t,TypeVar(a)) =>{
        var d = new oneSubst()
        d.extending(TypeVar(a), t)
        new CoupleSubst(d,unify(c.tail))
      }
      case (TypeFun(a,b), TypeFun(x,y)) => {
        var d = (a,x) :: (b,y) :: c.tail
        unify(d)
      }
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
    }

  override def typeOf(t: Term): Type = try {
    val TypingResult(tp, c) = collect(Nil: Env, t)
    val s = unify(c)
    s(tp)
  } catch {
    case TypeError(msg) =>
      Console.println("type error: " + msg)
      null
  }

}
