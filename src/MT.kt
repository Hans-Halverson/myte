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

fun readFile(fileName: String): String {
    val input = BufferedReader(FileReader(fileName))

    // Read entire file into string
    val file = StringBuilder()
    var line = input.readLine()
    while (line != null) {
        file.append(line)
        file.append("\n")
        line = input.readLine()
    }

    input.close()

    return file.toString()
}

fun tokenizeFiles(fileNames: List<String>): List<List<Token>> {
    val allTokens: MutableList<List<Token>> = mutableListOf()
    for (fileName in fileNames) {
        // Tokenize the entire file
        val file = readFile(fileName)
        val tokens = createTokens(StringReader(file), fileName)
        if (tokens.size == 0) {
            continue
        }

        allTokens.add(tokens)
    }

    return allTokens
}

/**
 * Run the REPL with input stream coming from the input reader.
 */
fun repl(packageFiles: List<String>) {
    // Set up compilation pipeline objects
    var symbolTable = SymbolTable()
    val environment = Environment()

    val parser = Parser(symbolTable)
    val converter = AstToIrConverter(symbolTable)
    val eval = Evaluator(symbolTable, environment)

    var importContext = parser.importContext

    // Tokenize, parse, convert, type check, and evaluate all included packages
    try {
        val packageTokens = tokenizeFiles(packageFiles)
        val parseFilesResult = parser.parseFiles(packageTokens)
        val convertPackagesResult = converter.convertPackages(parseFilesResult)
        eval.evaluatePackages(convertPackagesResult)
    } catch (except: ExceptionWithLocation) {
        printExceptionWithLocation(except, readFile(except.location.fileName!!))
        return
    } catch (except: ExceptionWithoutLocation) {
        printExceptionWithoutLocation(except, packageFiles[0], readFile(packageFiles[0]))
        return
    }

    // Start reading from command line
    val input = BufferedReader(InputStreamReader(System.`in`))

    // Initialize parser
    parser.initRepl()

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

                try {
                    val tokens = createTokens(StringReader(inputLines.toString()), null)

                    if (line.trim() == "") {
                        // If two blank lines in a row are seen, interpret as end of statement
                        if (seenBlankLine) {
                            println("Two empty lines encountered, ignoring input and moving to " +
                                    "next statement.")
                            continue@replLoop
                        // If a blank line is seen after an unambiguous end, it is simply a blank
                        // line and the next line of input should be processed. If the end is
                        // ambiguous, the input should be parsed normally as a complete statement.
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

                    // If line was just a comment, continue onto next statement
                    if (tokens.size == 0) {
                        continue@replLoop
                    }

                    // Try parsing current tokens, evaluate if successful. Otherwise gather tokens
                    // from next line and try parsing again.

                    // Create a new copy of symbol table and ident context and parse with it
                    val symbolTableCopy = symbolTable.copyForRepl()
                    val importContextCopy = importContext.copyForRepl()
                    parser.resetForReplLine(symbolTableCopy, importContextCopy, seenBlankLine)
                    converter.resetForReplLine(symbolTableCopy)
                    eval.resetForReplLine(symbolTableCopy)

                    // Parse a single line of repl input, process the statement, and evaluate
                    val parseReplLineResult = parser.parseReplLine(tokens)
                    val convertReplLineResult = converter.convertReplLine(parseReplLineResult)
                    eval.evaluateReplLine(convertReplLineResult, converter.typeChecker)

                    // Save the successfully updated symbol table and import context
                    symbolTable = parser.symbolTable
                    importContext = parser.importContext

                    continue@replLoop
                } catch (e: UnexpectedEOFException) {
                    continue@inputLoop
                } catch (e: AmbiguousEndException) {
                    ambiguousEnd = true
                    continue@inputLoop
                }
            } catch (except: ExceptionWithLocation) {
                printExceptionWithLocation(except, inputLines.toString())
                continue@replLoop
            } catch (except: ExceptionWithoutLocation) {
                printExceptionWithoutLocation(except, null, inputLines.toString())
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
fun evaluateFiles(fileNames: List<String>, packageNames: List<String>, args: List<String>): Int {
    // Set up symbol table and environment, and add all builtins
    val symbolTable = SymbolTable()
    val environment = Environment()

    // Set up parser, converter, and evaluator
    val parser = Parser(symbolTable)
    val converter = AstToIrConverter(symbolTable)
    val eval = Evaluator(symbolTable, environment)
    
    try {
        // Tokenize, parse, convert, type check, and evaluate all packages
        val packageTokens = tokenizeFiles(packageNames)
        val parsePackagesResult = parser.parseFiles(packageTokens)
        val convertPackagesResult = converter.convertPackages(parsePackagesResult)
        eval.evaluatePackages(convertPackagesResult)

        // Tokenize, parse, convert, type check, and evaluate all files
        val fileTokens = tokenizeFiles(fileNames)
        val parseFilesResult = parser.parseFiles(fileTokens)
        val convertFilesResult = converter.convertFiles(parseFilesResult)
        val mainReturnValue = eval.evaluateFiles(convertFilesResult, args)

        return mainReturnValue
    } catch (except: ExceptionWithLocation) {
        printExceptionWithLocation(except, readFile(except.location.fileName!!))
        return 1
    } catch (except: ExceptionWithoutLocation) {
        printExceptionWithoutLocation(except, fileNames[0], readFile(fileNames[0]))
        return 1
    }
}

/**
 * Parse command line args formatted as [FILE_NAMES]* --args [ARGS]* --packages [PACKAGE_FILES]*
 * into the list of files, command line arguments, and package files to include.
 */
fun parseArguments(args: List<String>): Triple<List<String>, List<String>, List<String>> {
    val files: MutableList<String> = mutableListOf()
    val commandLineArgs: MutableList<String> = mutableListOf()
    val packages: MutableList<String> = mutableListOf()

    val argIndex = args.indexOf("--args")
    val packageIndex = args.indexOf("--packages")

    for (i in 0 until argIndex) {
        files.add(args[i])
    }

    for (i in argIndex + 1 until packageIndex) {
        commandLineArgs.add(args[i])
    }

    for (i in packageIndex + 1 until args.size) {
        packages.add(args[i])
    }

    return Triple(files, commandLineArgs, packages)
}

fun main(args: Array<String>) {
    val (fileNames, commandLineArgs, packageFiles) = parseArguments(args.toList())
    if (fileNames.size == 0) {
        repl(packageFiles)
    } else {
        val status = evaluateFiles(fileNames, packageFiles, commandLineArgs)
        exitProcess(status)
    }
}
