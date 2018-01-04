package myte.eval

import myte.eval.values.*
import myte.shared.*

class EvaluationException(
    message: String,
    context: Context
) : ExceptionWithContext(message, context)

class Return(val returnValue: Value) : Exception("Returned value ${returnValue}")

object Continue : Exception("Continue")

object Break : Exception("Break")
