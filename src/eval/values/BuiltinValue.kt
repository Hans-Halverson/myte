package myte.eval.values

import myte.shared.*

class BuiltinValue(
    val ident: Identifier,
    val func: (List<Value>) -> Value,
    type: FunctionType
) : Value(type) {
    override fun toString(): String = "${ident.name}: ${type}"
}
