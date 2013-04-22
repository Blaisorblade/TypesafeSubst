package Subst

trait Exp[T]

case class Var[T](name: String) extends Exp[T]
case class Num(n: Int) extends Exp[Int]
case object Succ extends Exp[Int => Int]

case class Lam[S, T](x: Var[S], e: Exp[T]) extends Exp[S => T]
case class App[S, T](f: Exp[S => T], e: Exp[S]) extends Exp[T]

/*
//Let's add lists.
case class Cons[T](car: Exp[T], cdr: Exp[List[T]]) extends Exp[List[T]]
case class Null[T]() extends Exp[List[T]]
 */

object Lang {
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
          if (w == v)
            t
          else {
            //This only works if w is not free in repl.
            //Otherwise, we need to use another w.
            //Lam(w, subst(v, repl)(body))

            //To avoid implementing freeVars, we just do another
            //substitution with a free variable.
            val x = freshVar[s]()
            val alphaRenamedBody = subst(w, x)(body)
            Lam(x, subst(v, repl)(alphaRenamedBody))
          }
        case Num(_) => t
        case Succ => t
      }
  def betaReduce[S, T](f: Lam[S, T], arg: Exp[S]): Exp[T] =
    subst(f.x, arg)(f.e)
}

object Examples {
  //import Lang._

  val v = Var[Int]("v")
  val f = Lam(v, v)
  val a = App(f, Num(1))

  //val sound = eval(a, Empty)
}
