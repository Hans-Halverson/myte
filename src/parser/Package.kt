package myte.parser

import myte.parser.ast.*
import myte.shared.*

class Package(
    val typeDefs: MutableList<TypeDefinitionExpression> = mutableListOf(),
    val statements: MutableList<Statement> = mutableListOf()
)
