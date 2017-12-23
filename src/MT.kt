package myte

import myte.eval.*
import myte.eval.builtins.*
import myte.ir.*
import myte.lexer.*
import myte.parser.*
import myte.parser.ast.*
import myte.shared.*

import java.io.BufferedReader
import java.io.FileReader
import java.io.InputStreamReader
import java.io.StringReader

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
        val tokens: MutableList<Token> = mutableListOf()
        var numLines = 0
        var seenBlankLine = false

        // The input loop processes a single line of input at a time. A single statement
        // may take multiple lines of input to complete.
        inputLoop@ while (true) {
            // If on first iteration print initial prompt, otherwise print continuation prompt.
            if (numLines == 0) {
                print(">> ")
            } else {
                print(".. ")
            }

            val line = input.readLine()
            numLines++

            // If EOF is encountered, no statement could be created. If an empty line
            // is encountered on the first line of this statement, continue on to next statement.
            if (line == null) {
                break@replLoop
            } else if (line == "" && numLines == 1) {
                continue@replLoop
            }

            val lineTokens = createTokens(StringReader(line))

            if (lineTokens.size == 0) {
                // If two blank lines in a row are seen, interpret as end of statement
                if (seenBlankLine) {
                    println("Two empty lines encountered, ignoring input and moving to next statement.")
                    continue@replLoop
                } else {
                    seenBlankLine = true
                    continue@inputLoop
                }
            } else {
                seenBlankLine = false
            }

            tokens.addAll(lineTokens)

            // Try parsing current tokens, evaluate if successful.
            // Otherwise gather tokens from next line and try parsing again.
            try {
                // Create a new copy of symbol table and parse with it
                val symbolTableCopy = symbolTable.copy()
                val parser = Parser(symbolTableCopy, tokens)
                converter.resetSymbolTable(symbolTableCopy)
                eval.resetSymbolTable(symbolTableCopy)

                // Parse a single line of repl input
                val statement = parser.parseLine()

                // Convert to ir and perform type checking
                val ir = converter.convert(statement)
                converter.inferTypes(listOf(ir))
                converter.assertIRStructure(ir)

                // Evaluate the current input
                val value = eval.evaluate(ir)
                printValue(value)

                // Save the successfully updated symbol table
                symbolTable = parser.symbolTable

                continue@replLoop
            } catch (e: ParseEOFException) {
                continue@inputLoop
            }

        }
    }

    // Print newline at end so that next console prompt appears on new line
    println()
}

/**
 * Evaluate an entire file at once, with input coming from the given input reader.
 */
fun evaluateFile(input: BufferedReader) {
    // Tokenize the entire file
    val tokens = createTokens(input)
    if (tokens.size == 0) {
        return
    }

    // Set up symbol table and environment, and add all builtins
    val symbolTable = SymbolTable()
    val environment = Environment()
    registerBuiltins(symbolTable, environment)
    
    // Set up parser, converter, and evaluator
    val parser = Parser(symbolTable, tokens)
    val converter = AstToIrConverter(symbolTable)
    val eval = Evaluator(symbolTable, environment)

    // Parse, convert, and type check all statements in the file
    val statements = parser.parseFile()
    val irNodes = statements.map(converter::convert)

    converter.inferTypes(irNodes)
    irNodes.forEach(converter::assertIRStructure)

    // Evaluate each statement in the file in order
    for (irNode in irNodes) {
        eval.evaluate(irNode)
    }
}

fun main(args: Array<String>) {
    if (args.size == 0) {
        val reader = BufferedReader(InputStreamReader(System.`in`))
        repl(reader)
    } else if (args.size == 1) {
        val reader = BufferedReader(FileReader(args[0]))
        evaluateFile(reader)
    } else {
        println("Usage: mt [file]")
    }
}
