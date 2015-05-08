package com.codesynergy

import com.aimia.solver.client.model
import com.aimia.solver.client.model.LinearExpr
import com.aimia.solver.client.model.Model.Builder
import com.codesynergy.ModelDSL.ConstraintSense.ConstraintSense
import com.codesynergy.ModelDSL.ObjectiveSense.ObjectiveSense
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

  implicit def stringToVariable(name: String): Variable = name.split(" ").toList match {
    case l if l.size == 1 => Variable(name = l.head)
    case l if l.size == 2 => Variable(l.head, l(1).toDouble)
    case _ => throw new IllegalArgumentException(s"A non-expected list with more than two items as given: $name")
  }

  implicit def stringToExpression(name: String): Expression = name.split(" ").toList match {
    case l if l.size == 1 => new Expression(1, Variable(l.head))
    case l if l.size == 2 => new Expression(l.head.toDouble, Variable(l(1)))
    case _ => throw new IllegalArgumentException(s"A non-expected list with more than two items as given: $name")
  }

  implicit def intToVar(i: Int): Variable = Variable(coeff = i)

  implicit def doubleToVar(d: Double): Variable = Variable(coeff = d)

  implicit def variableToExpression(v: Variable): Expression = new Expression(1, v)

  implicit def varSenseToJavaVarType(s: VariableSense): model.Variable.Type = {
    s match {
      case VariableSense.binary => model.Variable.Type.BINARY
      case VariableSense.continuous => model.Variable.Type.CONTINUOUS
    }
  }

  implicit def constraintSenseToJavaConstraintSense(s: ConstraintSense): model.Constraint.Sense = {
    s match {
      case ConstraintSense.== => model.Constraint.Sense.EQUAL
      case ConstraintSense.<= => model.Constraint.Sense.LESS_EQUAL
      case ConstraintSense.>= => model.Constraint.Sense.GREATER_EQUAL
    }
  }

  implicit def objSenseToJavaObjSense(s: ObjectiveSense): model.Objective.Sense = {
    s match {
      case ObjectiveSense.maximize => model.Objective.Sense.MAXIMIZE
      case ObjectiveSense.minimize => model.Objective.Sense.MINIMIZE
    }
  }

  implicit def variableToJavaVariable(v: Variable): model.Variable = {
    val javaVariableBuilder = new model.Variable.Builder()
    javaVariableBuilder.withName(v.name)
    javaVariableBuilder.withLowerBound(v.lb.toDouble)
    javaVariableBuilder.withUpperBound(v.ub.toDouble)
    javaVariableBuilder.withType(v.sense)
    javaVariableBuilder.build()
  }

  implicit def constraintToJavaConsraint(c: Constraint): model.Constraint = {
    val javaConstraintBuilder = new model.Constraint.Builder()
    javaConstraintBuilder.withName(c.name)
    javaConstraintBuilder.withSense(c.sense)
    if (c.lhsVar != None) javaConstraintBuilder.withLhsVar(c.lhsVar.get)
    if (c.lhsExpr != None) javaConstraintBuilder.withLhsExpr(c.lhsExpr.get)
    if (c.lhsValue != None) javaConstraintBuilder.withLhsValue(c.lhsValue.get)
    if (c.rhsVar != None) javaConstraintBuilder.withRhsVar(c.rhsVar.get)
    if (c.rhsExpr != None) javaConstraintBuilder.withRhsExpr(c.rhsExpr.get)
    if (c.rhsValue != None) javaConstraintBuilder.withRhsValue(c.rhsValue.get)
    javaConstraintBuilder.build()
  }

  implicit def expressionToJavaExpression(e: Expression): model.LinearExpr = {
    val javaExpressionBuilder = new LinearExpr.Builder()
    e.vars.foreach(javaExpressionBuilder.addVariable(_))
    e.coeffs.foreach(javaExpressionBuilder.addCoefficient(_))
    javaExpressionBuilder.build()
  }

  implicit def objectiveToJavaObjective(o: Objective): model.Objective = {
    val javaObjectiveBuilder = new model.Objective.Builder()
    javaObjectiveBuilder.withLinearExp(o.expression)
    javaObjectiveBuilder.withSense(o.sense)
    javaObjectiveBuilder.build()
  }

  implicit def modelToJavaModel(m: Model): model.Model = {
    val javaModelBuilder = new Builder()
    javaModelBuilder.withName(m.name)
    m.constraints.foreach(javaModelBuilder.addConstraint(_))
    m.variables.foreach(javaModelBuilder.addVariable(_))
    javaModelBuilder
      .withObjective(m.objective)
      .build()
  }

  case class Model(name: String) {
    private var _sense: ObjectiveSense = _

    var objective: Objective = _
    var variables = new ArrayBuffer[Variable]()
    var constraints = new ArrayBuffer[Constraint]()

    def +=(v: Seq[Variable]): Model = {
      variables++=v
      this
    }

    def vars(v: Variable*): Model = {
      variables++=v
      this
    }

    def maximize(e: Expression): Model = {
      _sense = ObjectiveSense.maximize
      objective = Objective(e, ObjectiveSense.maximize)
      this
    }

    def minimize: Model = {
      _sense = ObjectiveSense.minimize
      this
    }

    def subject_to(c: Constraint): Model = {
      constraints+=c
      this
    }

    def convert = modelToJavaModel(this)
  }

  case class Variable(name: String = "", coeff: Double = 1.0) {

    private var _lb: Int = _
    private var _ub: Int = _
    private var variableSense: VariableSense = _

    def continuous(lb: Int, ub: Int) = {
      _lb = lb
      _ub = ub
      variableSense = VariableSense.continuous
      this
    }

    def binary(lb: Int, up: Int) = {
      _lb = lb
      _ub = ub
      variableSense = VariableSense.binary
      this
    }

    def * (coeff: Double): Expression = new Expression(coeff, this)

    def * (variable: Variable): Expression = new Expression(this.coeff, variable)

    def lb = _lb

    def ub = _ub

    def sense = variableSense
  }

  case class Constraint(name: String = "") {
    var lhsVar: Option[Variable] = None
    var rhsVar: Option[Variable] = None
    var lhsExpr: Option[Expression] = None
    var rhsExpr: Option[Expression] = None
    var lhsValue: Option[Double] = None
    var rhsValue: Option[Double] = None

    def this(lhsVar: Variable, rhsVar: Variable) = {
      this()
      this.lhsVar = Some(lhsVar)
      this.rhsVar = Some(rhsVar)
    }

    def this(lhsVar: Variable, rhsValue: Double) = {
      this()
      this.lhsVar = Some(lhsVar)
      this.rhsValue = Some(rhsValue)
    }

    def this(lhsVar: Variable, rhsExpr: Expression) = {
      this()
      this.lhsVar = Some(lhsVar)
      this.rhsExpr = Some(rhsExpr)
    }

    def this(lhsExpr: Expression, rhsExpr: Expression) = {
      this()
      this.lhsExpr = Some(lhsExpr)
      this.rhsExpr = Some(rhsExpr)
    }

    def this(lhsExpr: Expression, rhsValue: Double) = {
      this()
      this.lhsExpr = Some(lhsExpr)
      this.rhsValue = Some(rhsValue)
    }

    def this(lhsExpr: Expression, rhsVar: Variable) = {
      this()
      this.lhsExpr = Some(lhsExpr)
      this.rhsVar = Some(rhsVar)
    }

    def this(lhsValue: Double, rhsValue: Double) = {
      this()
      this.lhsValue = Some(lhsValue)
      this.rhsValue = Some(rhsValue)
    }

    def this(lhsValue: Double, rhsVar: Variable) = {
      this()
      this.lhsValue = Some(lhsValue)
      this.rhsVar = Some(rhsVar)
    }

    def this(lhsValue: Double, rhsExpr: Expression) = {
      this()
      this.lhsValue = Some(lhsValue)
      this.rhsExpr = Some(rhsExpr)
    }

    def sense: ConstraintSense.Value = null
  }

  case class Expression(coeff: Double, variable: Variable) {
    private var _vars: ArrayBuffer[Variable] = new ArrayBuffer[Variable]()
    private var _coeffs: ArrayBuffer[Double] = new ArrayBuffer[Double]()

    _coeffs += coeff
    _vars += variable

    def +(v: Variable*): Expression = {
      _vars ++= v
      v.foreach(_coeffs += _.coeff)
      this
    }

    def +(e: Expression): Expression = {
      _vars ++= e.vars
      _coeffs ++= e.coeffs
      this
    }

    def vars = _vars

    def coeffs = _coeffs

    def ==(rhsVal: Double): Constraint = new Constraint(this, rhsVal) with ==

    def <=(rhsVal: Double): Constraint = new Constraint(this, rhsVal) with <=

    def >=(rhsVal: Double): Constraint = new Constraint(this, rhsVal) with >=

    def ==(rhsVar: Variable): Constraint = new Constraint(this, rhsVar) with ==

    def <=(rhsVar: Variable): Constraint = new Constraint(this, rhsVar) with <=

    def >=(rhsVar: Variable): Constraint = new Constraint(this, rhsVar) with >=

    def ==(rhsExpr: Expression): Constraint = new Constraint(this, rhsExpr) with ==

    def <=(rhsExpr: Expression): Constraint = new Constraint(this, rhsExpr) with <=

    def >=(rhsExpr: Expression): Constraint = new Constraint(this, rhsExpr) with >=
  }

  case class Objective(expression: Expression, sense: ObjectiveSense)

  object ObjectiveSense extends Enumeration {
    type ObjectiveSense = Value
    val maximize, minimize = Value
  }

  trait < extends Constraint {
    override val sense = ConstraintSense.<
  }

  trait <= extends Constraint {
    override val sense = ConstraintSense.<=
  }

  trait > extends Constraint {
    override val sense = ConstraintSense.>
  }

  trait >= extends Constraint {
    override val sense = ConstraintSense.>=
  }

  trait == extends Constraint {
    override def sense = ConstraintSense.==
  }

  object ConstraintSense extends Enumeration {
    type ConstraintSense = Value
    val <, <=, >, >=, == = Value
  }

  object VariableSense extends Enumeration {
    type VariableSense = Value
    val continuous, binary = Value
  }

  def createModel = {
    val x: Variable = "x" continuous (0, 1)
    val y: Variable = "y" continuous (0, 1)
    val z: Variable = "z" continuous (0, 1)

    val obj: Expression = x + y + 2*z
    val c1: Constraint = x + 2*y + 3*z <= 10
    val c2: Constraint = x + y >= 1

    Model("simple-mip") vars (x, y, z) maximize obj subject_to c1 subject_to c2
  }

  def main(args: Array[String]) = {
    def javaModelToString(m: model.Model) = println(m)
    javaModelToString(createModel)
  }

}
