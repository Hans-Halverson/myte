package myte

import myte.eval.*
import myte.ir.*
import myte.lexer.*
import myte.parser.*
import myte.parser.ast.*

import java.io.BufferedReader
import java.io.FileReader
import java.io.InputStreamReader
import java.io.StringReader


fun repl(input: BufferedReader) {
	val parser = Parser()
	val converter = AstToIrConverter(parser.symbolTable)
	val eval = Evaluator(parser.symbolTable)

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
				parser.setTokens(tokens)
				val statement = parser.parseLine()

				val ir = converter.convert(statement)

				val value = eval.evaluate(ir)
				printValue(value)

				continue@replLoop
			} catch (e: ParseEOFException) {
				continue@inputLoop
			}

		}
	}

	// Print newline at end so that next console prompt appears on new line
	println()
}

fun evaluateFile(input: BufferedReader) {
	val tokens = createTokens(input)
	if (tokens.size == 0) {
		return
	}

	val parser = Parser(tokens)
	val converter = AstToIrConverter(parser.symbolTable)
	val statements = parser.parseFile()
	
	val eval = Evaluator(parser.symbolTable)

	for (statement in statements) {
		val ir = converter.convert(statement)
		val value = eval.evaluate(ir)
		printValue(value)
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
