package myte.eval

import myte.eval.values.*

class EvaluationException(message: String) : Exception(message)

class Return(val returnValue: Value) : Exception("Returned value ${returnValue}")
