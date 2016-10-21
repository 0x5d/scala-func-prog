package co.castillobgr

import State._

case class State[S,+A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))

}
