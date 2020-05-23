// See LICENSE for license details.

package midas.passes.fame

import java.io.{PrintWriter, File}

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.passes.MemPortUtils
import firrtl.analyses.InstanceGraph
import annotations.{InstanceTarget, Annotation, SingleTargetAnnotation}

import midas.targetutils.FirrtlFAMEModelAnnotation

import scala.collection.mutable
import mutable.{LinkedHashSet, LinkedHashMap}

class ExtractModel extends Transform {
  def inputForm = HighForm
  def outputForm = HighForm

  def promoteModels(state: CircuitState): CircuitState = {
    val fmAnns = state.annotations.collect({
      case ann: FirrtlFAMEModelAnnotation => ann.target.module -> ann
    }).toMap
    // Pick by order of parent in linearization -- don't pick children of main
    val modOrder = (new InstanceGraph(state.circuit)).moduleOrder.filterNot(_.name == state.circuit.main)
    println(modOrder.map(_.name))
    val topModelAnn = modOrder.collectFirst(Function.unlift(dm => fmAnns.get(dm.name)))
    topModelAnn.map({
      case FirrtlFAMEModelAnnotation(it) =>
        val anns = PromoteSubmoduleAnnotation(it) +: state.annotations
        println(s"Promoting ${it.module}.${it.instance} of ${it.ofModule}")
        promoteModels((new PromoteSubmodule).runTransform(state.copy(annotations = anns)))
    }).getOrElse(state)
  }

  override def execute(state: CircuitState): CircuitState = {
    promoteModels(state)
  }
}
