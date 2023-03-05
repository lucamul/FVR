package compiler.verification

import compiler.irs.Asts.VariableRef

import scala.collection.mutable

/**
 * Mutable uid generator for variables
 */
final class VarsCtx {
  private val variables = mutable.Map.empty[String, Int]

  /**
   * Generates a new uid for a variable named `rawName`, and sets it as the current uid for `rawName`
   * @return the new uid
   */
  def newNameFor(rawName: String): String = {
    val oldIdx = variables.getOrElse(rawName, -1)
    val newIdx = oldIdx + 1
    variables(rawName) = newIdx
    makeName(rawName, newIdx)
  }

  /**
   * If `rawName` is known by this `VarsCtx`, returns the current uid for `rawName`
   * 
   * O.w. has the same behavior as [[newNameFor]]
   */
  def nameFor(rawName: String): String = {
    variables.get(rawName) match
      case Some(idx) =>
        makeName(rawName, idx)
      case None =>
        newNameFor(rawName)
  }

  /**
   * @return an immutable map from raw names to uids corresponding to the current state of this `VarsCtx`
   */
  def currentRenameMapSnapshot: Map[String, VariableRef] = {
    variables.toMap
      .map((rawName, idx) => (rawName, VariableRef(makeName(rawName, idx))))
  }

  /**
   * @return a view on this `VarsCtx` that implements the `Map` interface but only defines its `get` method, and 
   *         has side effects on this `VarsCtx`
   *         
   * The `get(key)` method of the returned map behaves like `nameFor` (which it calls). It always returns a `Some`, 
   * containing the current uid for `key`, updating this `VarsCtx` with a new mapping `key` -> uid if `key` it does 
   * not know `key`
   */
  def currRenameMapView: Map[String, VariableRef] = {
    /*
     * Return a custom view so that the variables encountered during the traversal of 
     * the tree being renamed are added to this VarsCtx
     */
    new Map[String, VariableRef]{
      override def removed(key: String): Map[String, VariableRef] = {
        throw new UnsupportedOperationException()
      }

      override def updated[V1 >: VariableRef](key: String, value: V1): Map[String, V1] = {
        throw new UnsupportedOperationException()
      }

      override def get(key: String): Option[VariableRef] = {
        Some(VariableRef(nameFor(key)))
      }

      override def iterator: Iterator[(String, VariableRef)] = {
        throw new UnsupportedOperationException()
      }
    }
  }
  
  def copied: VarsCtx = {
    val newVarsCtx = new VarsCtx()
    newVarsCtx.variables.addAll(variables)
    newVarsCtx
  }
  
  private def makeName(rawName: String, idx: Int): String = {
    if idx == 0 then rawName
    else rawName ++ "%" ++ idx.toString
  }
  
}
