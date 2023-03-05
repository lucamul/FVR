package compiler.renamer

import compiler.renamer.{GlobalVarsCtx, LocalVarsCtx}
import lang.Types.Type

import scala.collection.mutable

final class LocalVarsCtx(private val globalVarsCtx: GlobalVarsCtx) {

  private val variables = mutable.Map.empty[String, String]
  
  def currNameFor(rawName: String): String = {
    variables.get(rawName) match
      case Some(name) => name
      case None => throw new NoSuchElementException(s"not found: $rawName")
  }
  
  def addAndRetNameFor(rawName: String, tpe: Type): String = {
    val name = globalVarsCtx.addVarRetName(rawName, tpe)
    variables(rawName) = name
    name
  }
  
  def copied: LocalVarsCtx = {
    val newLvc = new LocalVarsCtx(globalVarsCtx)
    newLvc.variables.addAll(variables)
    newLvc
  }

}

object LocalVarsCtx {

  def computeChanges(oldCtx: LocalVarsCtx, newCtx: LocalVarsCtx): Map[String, String] = {
    oldCtx.variables.keySet.intersect(newCtx.variables.keySet).map { rawVarName =>
      oldCtx.currNameFor(rawVarName) -> newCtx.currNameFor(rawVarName)
    }.toMap
  }

}

