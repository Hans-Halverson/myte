package myte.eval.values

import myte.ir.nodes.*
import myte.shared.*

data class Closure(val formalArgs: List<Identifier>, val stmt: IRNode, val environment: Environment) : Value()
