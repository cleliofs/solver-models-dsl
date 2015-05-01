package com.codesynergy

import com.aimia.solver.client.model.Model.Builder
import com.codesynergy.ModelDSL.ConstraintSense.ConstraintSense
import com.codesynergy.ModelDSL.ModelSense.ModelSense
import com.codesynergy.ModelDSL.VariableSense.VariableSense

import scala.collection.mutable.ArrayBuffer

/**
 * This example formulates and solves the following simple MIP model:
 *
 *  maximize    x + y + 2z
 *
 *  subject to  x + 2y + 3z <= 10
 *  subject to  x + y >= 1
 *              x, y, z continuous
 *
 * Created by csouza on 01/05/2015.
 */
object ModelDSL {

  implicit def stringToVariable(name: String): Variable = Variable(name)

  implicit def intToVariable(i: Int): Variable = Variable("")

  implicit def modelToJavaModel(model: Model): com.aimia.solver.client.model.Model = {
    var javaModelBuilder = new Builder();
    model.constraints.foreach(javaModelBuilder.addConstraint(_))
    model.variables.foreach(javaModelBuilder.addVariable(_))
    javaModelBuilder
      .withObjective(model.objective)
      .build()
  }

  case class Model(name: String) {
    private var _sense: ModelSense = _
    var objective: Expression = _
    var variables: ArrayBuffer[Variable] = _
    var constraints: ArrayBuffer[Constraint] = _

    def +=(v: Seq[Variable]): Model = {
      variables++=v
      this
    }

    def vars(v: Variable*): Model = {
      variables++=v
      this
    }

    def maximize(e: Expression): Model = {
      ._sense = ModelSense.maximize
      objective = e
      this
    }

    def minimize: Model = {
      _sense = ModelSense.minimize
      this
    }

    def subject_to(c: Constraint): Model = {
      constraints+=c
      this
    }

  }

  case class Variable(val name: String, var coeff: Double = 1.0) {

    private var _range: Range = _
    private var variableSense: VariableSense = _

    def continuous(range: Range = (0 to 1)) = {
      _range = range
      variableSense = VariableSense.continuous
      this
    }

    def coeff(c: Double): Variable = {
      coeff = c
      this
    }

    def binary(range: Range) = {
      _range = range
      variableSense = VariableSense.binary
      this
    }

    def +(v: Variable): Expression = new Expression(v)

  }

  case class Constraint(ex: Expression, constraintSense: ConstraintSense, rhsVal: Double)

  case class Expression(variable: Variable, name: String = "") {
    private var _exprs: ArrayBuffer[Variable] = _

    def +(v: Variable*): Expression = {
      _exprs ++= v
      this
    }

    def <=(rhsVal: Double): Constraint = Constraint(this, ConstraintSense.<=, rhsVal)

    def >=(rhsVal: Double): Constraint = Constraint(this, ConstraintSense.>=, rhsVal)
  }

  object ModelSense extends Enumeration {
    type ModelSense = Value
    val maximize, minimize = Value
  }

  object ConstraintSense extends Enumeration {
    type ConstraintSense = Value
    val <, <=, >, >=, == = Value
  }

  object VariableSense extends Enumeration {
    type VariableSense = Value
    val continuous, binary = Value
  }

  def main(args: Array[String]) = {
    val x: Variable = "x" continuous (0 to 1)
    val y: Variable = "y" continuous (0 to 1)
    val z: Variable = "z" continuous (0 to 1)

    //   maximize    x + y + 2z
    val obj: Expression = x + y + (z coeff 2) // x + y + 2z

    // subject to  x + 2y + 3z <= 10
    val c1: Constraint = (x + (y coeff 2) + (z coeff 3)) <= 10

    // subject to  x + y >= 1
    val c2: Constraint = (x + y) >= 1

    val model = Model("simple-mip") vars (x, y, z) maximize obj subject_to c1 subject_to c2
  }

}
