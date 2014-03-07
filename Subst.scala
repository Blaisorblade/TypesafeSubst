package Subst

import scala.language.higherKinds

trait Exp[T] extends Product

// We don't want structural but reference equality for variables, because the
// evaluator uses reference equality (through its typed matches against
// singleton types), so better not have the "wrong" equality available.
case class Var[T](name: String) extends Exp[T] {
  override def hashCode = System.identityHashCode(this)
  override def equals(a: Any) = super.equals(a)
}

object VarTest extends scala.App {
  val v1 = Var("")
  val v2 = Var("")
  val v3 = Var("")
  println(v1 == v2)
  println(v1 eq v2)
  println(v1.hashCode)
  println(v2.hashCode)
  println(v3.hashCode)
}

case class Num(n: Int) extends Exp[Int]
case object Succ extends Exp[Int => Int]

case class Lam[S, T](x: Var[S], e: Exp[T]) extends Exp[S => T]
case class App[S, T](f: Exp[S => T], e: Exp[S]) extends Exp[T]

/*
//Let's add lists.
case class Cons[T](car: Exp[T], cdr: Exp[List[T]]) extends Exp[List[T]]
case class Null[T]() extends Exp[List[T]]
 */

/**
 * Represent in Scala a particular kind of polymorphic functions called natural
 * transformations.
 * A natural transformation from A to B has type
 * forall T. A[T] => B[T].
 * This definition is also present in Scalaz/shapeless.
 *
 * A type-preserving rewrite rule can be given type Exp ~> Exp.
 */
trait ~>[-A[_], +B[_]] {
  def apply[T](a: A[T]): B[T]
}

//A version of UntypedTraversal working on typed trees.
trait TypedTraversal extends Reflection {
  def mapSubtrees(transformer: Exp ~> Exp) = new (Exp ~> Exp) {
    def apply[T](e: Exp[T]): Exp[T] = {
      val subtrees = e.productIterator.toList map {
        case subExp: Exp[t] => transformer(subExp)
        case notExp => notExp
      }
      reflectiveCopy(e, subtrees: _*)
    }
  }

  def traverse(transformer: Exp ~> Exp): Exp ~> Exp = new (Exp ~> Exp) {
    def apply[T](e: Exp[T]): Exp[T] = {
      transformer(mapSubtrees(traverse(transformer))(e))
    }
  }
}

object Lang extends TypedTraversal {
  /*
   * Avoid recreating vars.
   */
  override def reflectiveCopy[T <: Product](t: T, args: Any*): T = {
    (t, args) match {
      case (v @ Var(n), Seq(n2)) if n == n2 =>
        //This cast shouldn't be needed.
        v.asInstanceOf[T]
      case _ => super.reflectiveCopy(t, args: _*)
    }
  }

  var count = 0
  def freshVar[T](): Var[T] = {
    count += 1
    Var("x" + count)
  }

  def subst[S, T](v: Var[T], repl: Exp[T])(t: Exp[S]): Exp[S] =
      t match {
        //Cool! Same insight as in the other interpreter. Note that if I just
        //match names, it won't typecheck.
        case _: v.type =>
          repl
        case w: Var[t] =>
          assert(v.name != w.name)
          t
        case app: App[s, t] =>
          val fun = app.f
          val arg = app.e
          //I'd like to have a recursive subfunction taking just t,
          //but it's hard to write because it should also take S!
          App[s, t](subst(v, repl)(fun), subst(v, repl)(arg))
        case l: Lam[s, t] =>
          val w = l.x
          val body = l.e
          if (w eq v) //Must check identity, for coherency with variable lookup.
            t
          else {
            //This only works if w is not free in repl.
            //Otherwise, we need to use another w.
            //Lam(w, subst(v, repl)(body))

            //To avoid implementing freeVars, we just do another
            //substitution with a free variable.
            val x = freshVar[s]()
            val alphaRenamedBody = subst(w, x)(body)
            Lam[s, t](x, subst(v, repl)(alphaRenamedBody))
          }
        case Num(_) => t
        case Succ => t
      }
  def betaReduce[S, T](f: Lam[S, T], arg: Exp[S]): Exp[T] =
    subst(f.x, arg)(f.e)

  def betaReduceT[T](term: Exp[T]): Exp[T] =
    term match {
      case App(l: Lam[s, t], arg) => betaReduce(l, arg)
      case e => e
    }
  def betaReduceAnywhere =
    traverse(new (Exp ~> Exp) {
      def apply[T](term: Exp[T]): Exp[T] = betaReduceT(term)
    })
  def normalize[T](term: Exp[T]): Exp[T] = {
    val newT = betaReduceAnywhere(term)
    if (newT == term)
      newT
    else
      normalize(newT)
  }
}

object Examples {
  //import Lang._

  val v = Var[Int]("v")
  val f = Lam(v, v)
  val a = App(f, Num(1))

  val v2 = Var[Int]("v2")
  val trueF = Lam(v, Lam(v2, v))
  val falseF = Lam(v, Lam(v2, v2))
  val fV = Var[Int => Int => Int]("f")
  val test = Lam(fV, App(App(fV, Num(1)), Num(2)))
  val testTrue = App(test, trueF)
  val testFalse = App(test, falseF)
  //val sound = eval(a, Empty)
}
