package iliad

trait IliadError {
  def message: String
  override def toString: String = message
}
