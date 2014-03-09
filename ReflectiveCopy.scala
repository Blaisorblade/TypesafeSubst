package Subst

import scala.language.postfixOps

//I stole this back from Stefan's version here:
//https://github.com/fehrenbach/ilc/blob/426788b2817b956830cbdfdeda8ec7be43eabe80/scala-prototype/src/main/scala/ilc/feature/inference/Reflection.scala
//who altered it from my version. I should probably update my gist.

/*
In Scala, given the definition of an AST made of case classes, how do you define local transformations without
boilerplate for each case class? Here's my solution, based on reflection.
   -- Paolo
https://gist.github.com/Blaisorblade/827e357de942a46acbdb
 */

object Util {
  def count(amount: Int, noun: String): String = {
    (noun, amount) match {
      case (_, 1) => s"${amount} ${noun}"
      case (_, _) => s"${amount} ${noun}s"
    }
  }
}

import Util._

trait Reflection {
  /**
   * Allow functional update on arbitrary case classes. Call it with an
   * instance of a case class and its updated children.
   * Only works on case classes, or classes having an appropriate copy method.
   * The case class
   * @param t an instance of a case class
   */
  def reflectiveCopy[T <: Product](t: T, args: Any*): T = {
    val clazz = t.getClass
    if (args.length == t.productArity) {
      if (t.productArity == 0) {
        return t
      } else {
        val copyMethodOpt = clazz.getMethods filter (_.getName == "copy") headOption

        (copyMethodOpt getOrElse (
          throw new RuntimeException(s"No 'copy' method found in reflectiveCopy for ${t} of type ${clazz}")) invoke (t,
          args.toArray.asInstanceOf[Array[_ <: AnyRef]]: _*)).asInstanceOf[T]
      }
    } else {
      arityError(t.productArity, args.length)
    }
  }

  def arityError(expected: Int, given: Int) =
    throw new IllegalArgumentException(s"${count(expected, "argument")} expected but ${given} given")
}
