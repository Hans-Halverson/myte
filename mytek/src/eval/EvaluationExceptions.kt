package myte.eval

import myte.eval.values.*
import myte.shared.*

class EvaluationException(
    message: String,
    location: Location
) : ExceptionWithLocation(message, location)

class Return(val returnValue: Value) : Exception("Returned")

object Continue : Exception("Continue")

object Break : Exception("Break")
