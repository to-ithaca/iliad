package iliad
package gfx

import org.scalatest._

import cats._
import cats.implicits._

class UpdateTests extends FunSuite {

  Update[(ScopeProperty, Float)]
  Update[(ScopeProperty, Long => Float)]
  Update[(ScopeProperty, (Long, Float) => Long => Float)]
  Update[(ScopeProperty, (Long, Float) => Float)]
  Update[List[(ScopeProperty, Float)]]

}
