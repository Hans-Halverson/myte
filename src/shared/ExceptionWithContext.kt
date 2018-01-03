package myte.shared

/**
 * An exception that is associated with a certain context (line and character number where the
 * exception originates from).
 */
abstract class ExceptionWithContext(message: String, val context: Context) : Exception(message)

/**
 * A certain place in a file, represented by character and line number.
 */
data class Context(val charNum: Int, val lineNum: Int)

const val ESC = "\u001B"
const val RESET_ATTRIBUTES = ESC + "[0m"
const val BOLD_ATTRIBUTE = ESC + "[1m"
const val RED_COLOR = ESC + "[31m"
const val RESET_COLOR = ESC + "[39m"

/**
 * Print the message in the given exception, with the appropriate context displayed and
 * pointed to in the correct place.
 */
fun printExceptionWithContext(except: ExceptionWithContext, text: String) {
    var currentIdx = 0
    var currentLine = 1

    // Find the beginning of the correct line
    while (currentLine < except.context.lineNum) {
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

    // Print the error message formatted with terminal text color and highlighting
    println("${except.context.lineNum}:${except.context.charNum}:${BOLD_ATTRIBUTE}${RED_COLOR} " +
            "error: ${RESET_COLOR}${except.message}${RESET_ATTRIBUTES}")

    // Print out the offending line with a pointer below it to the exact character
    println(line)
    println("${" ".repeat(except.context.charNum - 1)}^")
}
