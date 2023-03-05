package compiler.renamer

import lang.Types.Type

import scala.collection.mutable

final class GlobalVarsCtx {
  private val variables = mutable.Map.empty[String, (Int, Type)]

  def addVarRetIdx(rawName: String, tpe: Type): Int = {
    val oldIdx = variables.getOrElse(rawName, (-1, null))._1
    val newIdx = oldIdx + 1
    variables(rawName) = (newIdx, tpe)
    newIdx
  }

  def addVarRetName(rawName: String, tpe: Type): String = {
    val idx = addVarRetIdx(rawName, tpe)
    makeName(rawName, idx)
  }

  private def makeName(rawName: String, idx: Int): String = {
    require(idx >= 0)
    if (idx == 0) then rawName
    else s"$rawName@$idx"
  }

}
