package myte.ir

import myte.ir.nodes.IRNode
import myte.shared.Identifier

class ConvertFilesResult(val nodes: List<IRNode>, val main: Identifier)

class ConvertPackagesResult(val nodes: List<IRNode>)

class ConvertReplLineResult(val toEvaluate: List<IRNode>, val toProcess: List<IRNode>)
