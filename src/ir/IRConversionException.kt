package myte.ir

import myte.shared.*

class IRConversionException(
    message: String,
    location: Location
) : ExceptionWithLocation(message, location)
