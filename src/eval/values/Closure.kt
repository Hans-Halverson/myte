package myte.eval.values

import myte.ir.nodes.*
import myte.shared.*

data class Closure(val ident: Identifier, val formalArgs: List<Identifier>, val body: IRNode, val environment: Environment, val type: FunctionType) : Value()
