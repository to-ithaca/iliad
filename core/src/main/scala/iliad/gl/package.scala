package iliad

import freek._

package object gl {
  type GL = Load :|: Cache :|: Draw :|: Current :|: OpenGL :|: FXNil
  val GLProgram = freek.Program[GL]

  object GL extends GLFunctions {
    case class State(cache: Cache.State, current: Current.State)
  }
}
