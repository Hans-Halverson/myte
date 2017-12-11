package myte.eval.values

import myte.ir.nodes.*
import myte.shared.*

data class NumericClosure(val formalArgs: List<Identifier>, val expr: IRNumericNode, val environment: Environment) : Value()
