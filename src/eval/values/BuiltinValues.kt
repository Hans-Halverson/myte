package myte.eval.values

import myte.shared.*

class BuiltinValue(
    val func: (List<Value>) -> Value,
    type: FunctionType
) : Value(type) {
    override fun toString(): String = "<function>"
}

class BuiltinMethodValue(
    val func: (List<Value>, Value) -> Value,
    val receiver: Value,
    type: FunctionType
) : Value(type) {
    override fun toString(): String = "<function>"
}