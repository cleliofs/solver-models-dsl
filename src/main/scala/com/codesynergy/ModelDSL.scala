package com.codesynergy

import com.aimia.solver.client.model
import com.aimia.solver.client.model.LinearExpr
import com.aimia.solver.client.model.Model.Builder
import com.codesynergy.ModelDSL.ConstraintSense.ConstraintSense
import com.codesynergy.ModelDSL.ObjectiveSense.ObjectiveSense
import com.codesynergy.ModelDSL.VariableSense.VariableSense

import scala.collection.mutable.ArrayBuffer

/**
 * OptZ++ a DSL in Scala for modelling LP models.
 *
 * Created by csouza on 01/05/2015.
 */
object ModelDSL {

  implicit def string2Variable(name: String): Variable = Variable(name)

  implicit def variable2Expression(v: Variable): Expression = new Expression(Some(1), Some(v))

  implicit def varSense2JavaVarType(s: VariableSense): model.Variable.Type = {
    s match {
      case VariableSense.binary => model.Variable.Type.BINARY
      case VariableSense.continuous => model.Variable.Type.CONTINUOUS
    }
  }

  implicit def constraintSense2JavaConstraintSense(s: ConstraintSense): model.Constraint.Sense = {
    s match {
      case ConstraintSense.== => model.Constraint.Sense.EQUAL
      case ConstraintSense.<= => model.Constraint.Sense.LESS_EQUAL
      case ConstraintSense.>= => model.Constraint.Sense.GREATER_EQUAL
    }
  }

  implicit def objSense2JavaObjSense(s: ObjectiveSense): model.Objective.Sense = {
    s match {
      case ObjectiveSense.maximize => model.Objective.Sense.MAXIMIZE
      case ObjectiveSense.minimize => model.Objective.Sense.MINIMIZE
    }
  }

  implicit def variable2JavaVariable(v: Variable): model.Variable = {
    val javaVariableBuilder = new model.Variable.Builder()
    javaVariableBuilder.withName(v.name)
    javaVariableBuilder.withLowerBound(v.lb.toDouble)
    javaVariableBuilder.withUpperBound(v.ub.toDouble)
    javaVariableBuilder.withType(v.sense)
    javaVariableBuilder.build()
  }

  implicit def constraint2JavaConsraint(c: Constraint): model.Constraint = {
    val javaConstraintBuilder = new model.Constraint.Builder()
    javaConstraintBuilder.withName(c.name)
    javaConstraintBuilder.withSense(c.sense)
    if (c.lhsVar.isDefined) javaConstraintBuilder.withLhsVar(c.lhsVar.get)
    if (c.lhsExpr.isDefined) javaConstraintBuilder.withLhsExpr(c.lhsExpr.get)
    if (c.lhsValue.isDefined) javaConstraintBuilder.withLhsValue(c.lhsValue.get)
    if (c.rhsVar.isDefined) javaConstraintBuilder.withRhsVar(c.rhsVar.get)
    if (c.rhsExpr.isDefined) javaConstraintBuilder.withRhsExpr(c.rhsExpr.get)
    if (c.rhsValue.isDefined) javaConstraintBuilder.withRhsValue(c.rhsValue.get)
    javaConstraintBuilder.build()
  }

  implicit def expression2JavaExpression(e: Expression): model.LinearExpr = {
    val javaExpressionBuilder = new LinearExpr.Builder()
    e.vars.foreach(javaExpressionBuilder.addVariable(_))
    e.coeffs.foreach(javaExpressionBuilder.addCoefficient(_))
    javaExpressionBuilder.withConstant(null).build()
  }

  implicit def objective2JavaObjective(o: Objective): model.Objective = {
    val javaObjectiveBuilder = new model.Objective.Builder()
    javaObjectiveBuilder.withLinearExp(o.expression)
    javaObjectiveBuilder.withSense(o.sense)
    javaObjectiveBuilder.build()
  }

  implicit def model2JavaModel(m: Model): model.Model = {
    val javaModelBuilder = new Builder()
    javaModelBuilder.withName(m.name)
    m.constraints.foreach(javaModelBuilder.addConstraint(_))
    m.variables.foreach(javaModelBuilder.addVariable(_))
    javaModelBuilder
      .withObjective(m.objective)
      .build()
  }


  case class Model(name: String) {
    var objective: Objective = _
    var variables = new ArrayBuffer[Variable]()
    var constraints = new ArrayBuffer[Constraint]()

    def +=(v: Seq[Variable]): Model = {
      variables++=v
      this
    }

    def vars(variableSeq: Seq[Variable]): Model = {
      variables++=variableSeq
      this
    }

    def maximize(e: Expression): Model = {
      objective = Objective(e, ObjectiveSense.maximize)
      this
    }

    def minimize(e: Expression): Model = {
      objective = Objective(e, ObjectiveSense.minimize)
      this
    }

    def subject_to(constraintSeq: Seq[Constraint]): Model = {
      constraints++=constraintSeq
      this
    }

    def convert = model2JavaModel(this)
  }

  case class Variable(name: String = "") {

    private var _lb: Int = _
    private var _ub: Int = _
    private var _obj: Double = _
    private var variableSense: VariableSense = _

    def continuous(lb: Int, ub: Int) = {
      _lb = lb
      _ub = ub
      variableSense = VariableSense.continuous
      this
    }

    def continuous(lb: Double, ub: Double): Variable = {
      continuous(lb.toInt, ub.toInt)
    }

    def continuous(lb: String, ub: String): Variable = {
      continuous(lb.toDouble, ub.toDouble)
    }

    def binary(lb: Int, ub: Int) = {
      _lb = lb
      _ub = ub
      variableSense = VariableSense.binary
      this
    }

    def binary(lb: Double, ub: Double): Variable = {
      binary(lb.toInt, ub.toInt)
    }

    def objective(obj: Double) = {
      _obj = obj
      this
    }

    def objective(obj: String): Variable = {
      objective(obj.toDouble)
    }

    def *:(coeff: Double): Expression = new Expression(Some(coeff), Some(this))

    def *:(coeff: String): Expression = new Expression(Some(coeff.toDouble), Some(this))

    def lb = _lb

    def ub = _ub

    def obj = _obj

    def sense = variableSense
  }

  abstract class Constraint(var name: String = "") {
    var lhsVar: Option[Variable] = None
    var rhsVar: Option[Variable] = None
    var lhsExpr: Option[Expression] = None
    var rhsExpr: Option[Expression] = None
    var lhsValue: Option[Double] = None
    var rhsValue: Option[Double] = None

    def name(n: String): Constraint = {
      name = n
      this
    }

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

    val sense: ConstraintSense.Value
  }

  case class Expression(coeff: Option[Double], variable: Option[Variable]) {
    private var _vars: ArrayBuffer[Variable] = new ArrayBuffer[Variable]()
    private var _coeffs: ArrayBuffer[Double] = new ArrayBuffer[Double]()

    if (coeff.isDefined) _coeffs += coeff.get
    if (variable.isDefined) _vars += variable.get

    def this() {
      this(None, None)
    }

    def +(e: Expression): Expression = {
      _vars ++= e.vars
      _coeffs ++= e.coeffs
      this
    }

    def vars = _vars

    def coeffs = _coeffs

    // operators for rhs value
    def ==(rhsVal: Double) = new Constraint(this, rhsVal) with ==

    def <=(rhsVal: Double) = new Constraint(this, rhsVal) with <=

    def >=(rhsVal: Double) = new Constraint(this, rhsVal) with >=

    def <(rhsVal: Double) = new Constraint(this, rhsVal) with <

    def >(rhsVal: Double) = new Constraint(this, rhsVal) with >


    // operators for rhs variable
    def ==(rhsVar: Variable) = new Constraint(this, rhsVar) with ==

    def <=(rhsVar: Variable) = new Constraint(this, rhsVar) with <=

    def >=(rhsVar: Variable) = new Constraint(this, rhsVar) with >=

    def <(rhsVar: Variable) = new Constraint(this, rhsVar) with <

    def >(rhsVar: Variable) = new Constraint(this, rhsVar) with >


    // operators for rhs expression
    def ==(rhsExpr: Expression) = new Constraint(this, rhsExpr) with ==

    def <=(rhsExpr: Expression) = new Constraint(this, rhsExpr) with <=

    def >=(rhsExpr: Expression) = new Constraint(this, rhsExpr) with >=

    def <(rhsExpr: Expression) = new Constraint(this, rhsExpr) with <

    def >(rhsExpr: Expression) = new Constraint(this, rhsExpr) with >
  }

  case class Objective(expression: Expression, sense: ObjectiveSense)

  object ObjectiveSense extends Enumeration {
    type ObjectiveSense = Value
    val maximize, minimize = Value
  }

  trait ConstraintSenseOperator {
    val sense: ConstraintSense.Value
  }

  trait < extends ConstraintSenseOperator {
    val sense = ConstraintSense.<
  }

  trait <= extends ConstraintSenseOperator {
    val sense = ConstraintSense.<=
  }

  trait > extends ConstraintSenseOperator {
    val sense = ConstraintSense.>
  }

  trait >= extends ConstraintSenseOperator {
    val sense = ConstraintSense.>=
  }

  trait == extends ConstraintSenseOperator {
    val sense = ConstraintSense.==
  }

  object ConstraintSense extends Enumeration {
    type ConstraintSense = Value
    val <, <=, >, >=, == = Value
  }

  object VariableSense extends Enumeration {
    type VariableSense = Value
    val continuous, binary = Value
  }


}
