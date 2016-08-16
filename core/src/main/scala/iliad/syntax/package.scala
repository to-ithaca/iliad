package iliad

package object syntax {
  object all extends AllSyntax

  object vectord extends VectorDSyntax
  object matrixd extends MatrixDSyntax
  object point extends PointInstances
  object tap extends TapInstances
  object swipe extends SwipeInstances
  object drag extends DragContinuedInstances
}
