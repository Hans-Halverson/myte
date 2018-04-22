package myte

import myte.eval.*
import myte.eval.builtins.*
import myte.eval.values.*
import myte.ir.*
import myte.ir.nodes.*
import myte.lexer.*
import myte.parser.*
import myte.parser.ast.*
import myte.shared.*

import java.io.BufferedReader
import java.io.FileReader
import java.io.InputStreamReader
import java.io.StringReader

import kotlin.system.exitProcess

/**
 * Run the REPL with input stream coming from the input reader.
 */
fun repl(input: BufferedReader) {
    var symbolTable = SymbolTable()
    val environment = Environment()
    registerBuiltins(symbolTable, environment)

    val converter = AstToIrConverter(symbolTable)
    val eval = Evaluator(symbolTable, environment)

    // The repl loop process a single input to the repl, consisting of a 
    // single statement which will be evaluated.
    replLoop@ while (true) {
        val inputLines = StringBuilder()
        var numLines = 0
        var seenBlankLine = false
        var ambiguousEnd = false

        // The input loop processes a single line of input at a time. A single statement
        // may take multiple lines of input to complete.
        inputLoop@ while (true) {
            try {
                // If on first iteration print initial prompt, otherwise print continuation prompt.
                if (numLines == 0) {
                    print(">> ")
                } else {
                    print("   ")
                }

                val line = input.readLine()

                // Add the new line with the end of line character removed by readLine
                inputLines.append(line)
                inputLines.append("\n")

                numLines++

                // If EOF is encountered, no statement could be created. If an empty line is
                // encountered on the first line of this statement, continue on to next statement.
                if (line == null) {
                    break@replLoop
                } else if (line == "" && numLines == 1) {
                    continue@replLoop
                }

                val tokens = createTokens(StringReader(inputLines.toString()), null)

                if (line.trim() == "") {
                    // If two blank lines in a row are seen, interpret as end of statement
                    if (seenBlankLine) {
                        println("Two empty lines encountered, ignoring input and moving to " +
                                "next statement.")
                        continue@replLoop
                    // If a blank line is seen after an unambiguous end, it is simply a blank line
                    // and the next line of input should be processed. If the end is ambiguous,
                    // the input should be parsed normally as a complete statement.
                    } else {
                        seenBlankLine = true
                        if (!ambiguousEnd) {
                            continue@inputLoop
                        }
                    }
                } else {
                    seenBlankLine = false
                }

                ambiguousEnd = false

                // Try parsing current tokens, evaluate if successful.
                // Otherwise gather tokens from next line and try parsing again.
                try {
                    // Create a new copy of symbol table and parse with it
                    val symbolTableCopy = symbolTable.copy()
                    val parser = Parser(symbolTableCopy, tokens, seenBlankLine)
                    converter.resetSymbolTable(symbolTableCopy)
                    eval.resetSymbolTable(symbolTableCopy)

                    // Parse a single line of repl input
                    val statement = parser.parseReplLine()

                    if (statement != null) {
                        // Convert to ir and perform type checking
                        val ir = converter.convert(statement)
                        converter.inferTypes(listOf(ir))
                        converter.assertIRStructure(ir)

                        // Evaluate the current input
                        val value = eval.evaluate(ir)
                        printValue(value)
                    }

                    // Save the successfully updated symbol table
                    symbolTable = parser.symbolTable

                    continue@replLoop
                } catch (e: ParseEOFException) {
                    continue@inputLoop
                } catch (e: AmbiguousEndException) {
                    ambiguousEnd = true
                    continue@inputLoop
                }
            } catch (except: ExceptionWithLocation) {
                printExceptionWithLocation(except, inputLines.toString())
                continue@replLoop
            }
        }
    }

    // Print newline at end so that next console prompt appears on new line
    println()
}

/**
 * Evaluate an entire file at once, with input coming from the given input reader.
 *
 * @return an int success value - 0 if successful, or another int on error
 */
fun evaluateFile(input: BufferedReader, fileName: String, args: List<String>): Int {
    // Read entire file into string
    val file = StringBuilder()
    var line = input.readLine()
    while (line != null) {
        file.append(line)
        file.append("\n")
        line = input.readLine()
    }

    // Tokenize the entire file
    val tokens = createTokens(StringReader(file.toString()), fileName)
    if (tokens.size == 0) {
        return 0
    }

    // Set up symbol table and environment, and add all builtins
    val symbolTable = SymbolTable()
    val environment = Environment()
    registerBuiltins(symbolTable, environment)
    
    try {
        // Set up parser, converter, and evaluator
        val parser = Parser(symbolTable, tokens)
        val converter = AstToIrConverter(symbolTable)
        val eval = Evaluator(symbolTable, environment)

        // Parse, convert, and type check all statements in the file
        val statements = parser.parseFile()
        val irNodes = statements.map(converter::convert)

        converter.inferTypes(irNodes)
        irNodes.forEach(converter::assertIRStructure)

        // Evaluate each statement in the file in order, saving the main function
        var mainFunc: ClosureValue? = null
        for (irNode in irNodes) {
            eval.evaluate(irNode)
            if (irNode is FunctionDefinitionNode && irNode.ident.name == "main") {
                mainFunc = eval.environment.lookup(irNode.ident) as ClosureValue
            }
        }

        // Error if a main function is not found
        if (mainFunc == null) {
            println("Main function must exist")
            return 1
        }

        // Call main function on input arguments
        val argValues = VectorValue(args.map({ str -> StringValue(str) }).toMutableList(),
                VectorType(StringType))
        val returnValue = eval.applyClosureToArgs(mainFunc, listOf(argValues))
        if (returnValue == null || returnValue !is IntValue) {
            println("Main function must return an int")
            return 1
        }

        return returnValue.num
    } catch (except: ExceptionWithLocation) {
        printExceptionWithLocation(except, file.toString())
        return 1
    }
}

fun main(args: Array<String>) {
    if (args.size == 0) {
        val reader = BufferedReader(InputStreamReader(System.`in`))
        repl(reader)
    } else {
        val reader = BufferedReader(FileReader(args[0]))
        val status = evaluateFile(reader, args[0], args.toList().drop(1))
        exitProcess(status)
    }
}
