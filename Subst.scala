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
        //Cool! Same insight as in the other interpreter.
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
        case Lam(w, body) =>
          if (w == v)
            t
          else {
            //This only works if w is not free in repl.
            //Otherwise, we need to use another w.
            //Lam(w, subst(v, repl)(body))
            //To avoid implementing substitution
            val x = freshVar[Any]() //Luckily we need this annotation.
                                  //That's just God slapping in our face the fact that
                                  //this is not well-typed.
            Lam(x, subst(v, repl)(subst(w, x)(body)))
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
