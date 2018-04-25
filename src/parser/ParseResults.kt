package myte.parser

import myte.parser.ast.TopLevelStatement

class ParseFilesResult(val packages: List<Package>, val importContexts: List<ImportContext>)

class ParseReplLineResult(val statement: TopLevelStatement?, val importContext: ImportContext)
