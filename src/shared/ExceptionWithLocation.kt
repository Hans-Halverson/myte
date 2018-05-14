package myte.shared

/**
 * An exception that is associated with a certain location (line and character number where the
 * exception originates from).
 */
open class ExceptionWithLocation(message: String, val location: Location) : Exception(message)

/**
 * An exception that should be displayed in the same way as with a location, but where no
 * location exists.
 */
open class ExceptionWithoutLocation(message: String) : Exception(message)

class UnexpectedEOFException() : Exception("Unexpected EOF encountered")

/**
 * A certain place in a file, represented by character and line number.
 */
data class Location(val charNum: Int, val lineNum: Int, val fileName: String?)

val NO_LOCATION = Location(1, 1, null)

const val ESC = "\u001B"
const val RESET_ATTRIBUTES = ESC + "[0m"
const val BOLD_ATTRIBUTE = ESC + "[1m"
const val RED_COLOR = ESC + "[31m"
const val RESET_COLOR = ESC + "[39m"

/**
 * Print the message in the given exception, with the appropriate location displayed and
 * pointed to in the correct place.
 */
fun printExceptionWithLocation(except: ExceptionWithLocation, text: String) { 
    var currentIdx = 0
    var currentLine = 1

    // Find the beginning of the correct line
    while (currentLine < except.location.lineNum) {
        if (text.get(currentIdx) == '\n') {
            currentLine++
        }

        currentIdx++
    }

    // Find the end of the current line
    val lineBeginIdx = currentIdx
    while (text.get(currentIdx) != '\n') {
        currentIdx++
    }

    // Pull out the entire line, and replace all tabs with four spaces
    val line = text.substring(lineBeginIdx, currentIdx).replace("\t", "    ")
    val fileNamePrefix = if (except.location.fileName != null) {
        "${except.location.fileName}:"
    } else {
        ""
    }

    // Print the error message formatted with terminal text color and highlighting
    println("${fileNamePrefix}${except.location.lineNum}:${except.location.charNum}:" +
            "${BOLD_ATTRIBUTE}${RED_COLOR} error: ${RESET_COLOR}${except.message}" +
            "${RESET_ATTRIBUTES}")

    // Print out the offending line with a pointer below it to the exact character
    println(line)
    println("${" ".repeat(except.location.charNum - 1)}^")
}

/**
 * Print an exception without location in the same format as an exception with location, at the
 * beginning of the specified file.
 */
fun printExceptionWithoutLocation(
    except: ExceptionWithoutLocation,
    fileName: String?,
    text: String
) {
    printExceptionWithLocation(ExceptionWithLocation(except.message!!,
            Location(1, 1, fileName)), text)
}
