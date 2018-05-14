package myte.eval.values

import myte.eval.*
import myte.shared.*

class BuiltinValue(
    val func: (List<Value>) -> Value,
    type: FunctionType
) : Value(type) {
    override fun toString(): String = "<function>"
}

class BuiltinMethodValue(
    val func: (List<Value>, Value, Environment, Evaluator) -> Value,
    val receiver: Value,
    type: FunctionType
) : Value(type) {
    override fun toString(): String = "<function>"
}