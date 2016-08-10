package iliad

import freek._

package object gl {
  type AST = Load :|: Cache :|: Draw :|: Current :|: OpenGL :|: FXNil
  val GLProgram = freek.Program[AST]
}
