package myte.ir

import myte.ir.nodes.*
import myte.shared.*

/**
 * A pattern with context contains in its first component the rest of the current pattern list,
 * and in its second component contains a stack of the rest of the pattern lists in all lower
 * nesting levels.
 */
typealias PatternWithContext = Pair<List<IRNode>, List<List<IRNode>>>

typealias MatchTrace = List<MatchTraceOption>

/**
 * A single step in the exhaustive match checking process. A full trace can be used to recreate
 * a value that is not matched.
 */
sealed class MatchTraceOption()

object VariableTraceOption : MatchTraceOption()
object UnitTraceOption : MatchTraceOption()
object TupleBeginTraceOption : MatchTraceOption()
object EndNestingLevelTraceOption : MatchTraceOption()

// A bool type, along with the bool value that was checked
class BoolTraceOption(val bool: Boolean) : MatchTraceOption()

// A byte type, along with all byte literals that were checked
class ByteTraceOption(val bytes: Set<Byte>) : MatchTraceOption()

// An int type, along with all int literals that were checked
class IntTraceOption(val ints: Set<Int>) : MatchTraceOption()

// A float type, along with all float literals that were checked
class FloatTraceOption(val floats: Set<Float>) : MatchTraceOption()

// A double type, along with all double literals that were checked
class DoubleTraceOption(val doubles: Set<Double>) : MatchTraceOption()

// A string type, along with all string literals that were checked
class StringTraceOption(val strings: Set<String>) : MatchTraceOption()

// A vector type, along with the size of all vector literals that were checked
class VectorTraceOption(val vectorSizes: Set<Int>) : MatchTraceOption()

// A set type, along with the size of all set literals that were checked
class SetTraceOption(val setSizes: Set<Int>) : MatchTraceOption()

// A map type, along with the size of all map literals that were checked
class MapTraceOption(val mapSizes: Set<Int>) : MatchTraceOption()

// A tuple variant type constructor, along with the variant that was checked
class TupleVariantBeginTraceOption(val variant: TupleVariant) : MatchTraceOption()

/**
 * A record variant type constructor, along with the variant that was checked and the fields that
 * were checked, in the order they were checked.
 */
class RecordVariantBeginTraceOption(
    val variant: RecordVariant,
    val fields: List<String>
) : MatchTraceOption()

class InexhaustiveMatchException(val trace: MatchTrace) : Exception()

val VARIABLE_NODE = VariableNode(Identifier("_"), NO_LOCATION)

/**
 * Check whether every match statement in the tree rooted at the given root has exhaustive match
 * cases for its type.
 */
fun exhaustiveMatchCases(root: IRNode) {
    root.forEach { node -> 
        if (node is MatchNode) {
            // Find all patterns without guards to check for exhaustive matching. Guarded cases
            // are ignored because the guard could evaluate to false at runtime.
            val patterns = node.cases.mapNotNull { (pattern, guard, _) ->
                if (guard == null) {
                    Pair<List<IRNode>, List<List<IRNode>>>(listOf(pattern), listOf())
                } else {
                    null
                }
            }

            // Error if all patterns have guards
            if (patterns.isEmpty()) {
                throw IRConversionException("Inexhaustive match case. All cases are guarded, " +
                        "there is no guarantee one will be matched at runtime.", node.startLocation)
            }

            try {
                exhaustiveMatches(listOf(node.expr.type), patterns, listOf(), listOf(), listOf())
            } catch (e: InexhaustiveMatchException) {
                val trace = traceToMatchedCase(e.trace, node.expr.type)
                throw IRConversionException("Inexhaustive match case. Here is an example of a " +
                        "case that is not matched: ${trace}", node.startLocation)
            }
        // If this is a pattern assignment node, check for exhaustive cases for single pattern
        } else if (node is PatternDefinitionNode) {
            val pattern = Pair<List<IRNode>, List<List<IRNode>>>(listOf(node.pattern), listOf())
            try {
                exhaustiveMatches(listOf(node.expr.type), listOf(pattern), listOf(), listOf(),
                        listOf())
            } catch (e: InexhaustiveMatchException) {
                val trace = traceToMatchedCase(e.trace, node.expr.type)
                throw IRConversionException("Inexhaustive pattern matching in definition. " +
                        "Here is an example of a case that is not matched: ${trace}",
                        node.startLocation)
            }
        // If this is a pattern definition node, check for exhaustive cases for single pattern
        } else if (node is PatternAssignmentNode) {
            val pattern = Pair<List<IRNode>, List<List<IRNode>>>(listOf(node.pattern), listOf())
            try {
                exhaustiveMatches(listOf(node.rValue.type), listOf(pattern), listOf(), listOf(),
                        listOf())
            } catch (e: InexhaustiveMatchException) {
                val trace = traceToMatchedCase(e.trace, node.rValue.type)
                throw IRConversionException("Inexhaustive pattern matching in assignment. " +
                        "Here is an example of a case that is not matched: ${trace}",
                        node.startLocation)
            }
        }
    }
}

/** 
 * Filter patterns which pass a certain predicate, keeping all VariableNodes.
 *
 * @param patterns a list of pattern contexts, a portion of which should be selected
 * @param pred a predicate that reports whether an IRNode should be chosen
 * @return a list of pattern contexts taken from patterns, where the first node in the current
 *         pattern passes the predicates. All VariableNodes are also kept.
 */
private fun filterPatterns(
    patterns: List<PatternWithContext>,
    pred: (IRNode) -> Boolean
): List<PatternWithContext> {
    return patterns.mapNotNull { (pat, rest) ->
        val firstPattern = pat[0]
        if (firstPattern is VariableNode || pred(firstPattern)) {
            Pair(pat.drop(1), rest)
        } else {
            null
        }
    }
}

/**
 * Determine whether a list of patterns exhaustively matches a given type.
 *
 * @param types the current list of types to match against
 * @param patterns the current pattern context
 * @param restOfTypes a stack of type lists, corresponding to the nested types at the current
 *        nesting level
 * @param varPatterns a stack of pattern contexts that correspond to the patterns which have
 *        variable patterns for the current nesting level
 * @return whether the list of patterns exhaustively matches the given types
 */
private fun exhaustiveMatches(
    types: List<Type>,
    patterns: List<PatternWithContext>,
    restOfTypes: List<List<Type>>,
    varPatterns: List<List<PatternWithContext>>,
    trace: MatchTrace
) {
    if (patterns.isEmpty()) {
        // If no possible patterns, but variables higher up the stack exist, then
        // continue into the lower nesting level
        if (varPatterns.any { vp -> !vp.isEmpty() }) {
            exhaustiveMatches(restOfTypes[0], varPatterns[0], restOfTypes.drop(1),
                    varPatterns.drop(1), trace + EndNestingLevelTraceOption)
            return
        }

        // Otherwise there cannot be a match
        throw InexhaustiveMatchException(trace)
    } else if (types.isEmpty()) {
        // If there are no current (or next) types, we have successfully matched all options
        if (restOfTypes.isEmpty()) {
            return
        // Otherwise continue on to lower nesting level
        } else {
            var restOfPatterns = patterns.map {(_, rest) ->
                Pair<List<IRNode>, List<List<IRNode>>>(rest[0], rest.drop(1))
            }

            exhaustiveMatches(restOfTypes[0], restOfPatterns + varPatterns[0], restOfTypes.drop(1),
                    varPatterns.drop(1), trace + EndNestingLevelTraceOption)
            return
        }
    }

    val typeToCheck = types[0]
    val nextTypes = types.drop(1)

    // Unit type can only match the unit literal (or a variable)
    if (typeToCheck is UnitType) {
        val unitPatterns = filterPatterns(patterns) { pat -> pat is UnitLiteralNode }
        exhaustiveMatches(nextTypes, unitPatterns, restOfTypes, varPatterns,
                trace + UnitTraceOption)
    // Bool type must have patterns for both true and false
    } else if (typeToCheck is BoolType) {
        val truePatterns = filterPatterns(patterns) {
            pat -> pat is BoolLiteralNode && pat.bool
        }

        val falsePatterns = filterPatterns(patterns) {
            pat -> pat is BoolLiteralNode && !pat.bool
        }

        exhaustiveMatches(nextTypes, truePatterns, restOfTypes, varPatterns,
                trace + BoolTraceOption(true))
        exhaustiveMatches(nextTypes, falsePatterns, restOfTypes, varPatterns,
                trace + BoolTraceOption(false))
    // Tuple types increase the nesting level
    } else if (typeToCheck is TupleType) {
        // All variable patterns are put on the varPattern stack for the next nesting level
        val vars = filterPatterns(patterns) { _ -> false }

        // There must be exhaustve options for tuple literals
        val tuplePatterns = patterns.mapNotNull { (pat, rest) -> 
            val firstPattern = pat[0]
            if (firstPattern is TupleLiteralNode) {
                Pair(firstPattern.elements, listOf(pat.drop(1)) + rest)
            } else {
                null
            }
        }

        exhaustiveMatches(typeToCheck.elementTypes, tuplePatterns, listOf(nextTypes) + restOfTypes,
                listOf(vars) + varPatterns, trace + TupleBeginTraceOption)
    // Algebraic data types increase the nesting level
    } else if (typeToCheck is AlgebraicDataType) {
        // All variable patterns are put on the varPattern stack for the next nesting levels
        val vars = filterPatterns(patterns) { _ -> false }

        // There must be exhaustive, nested matches for each variant in this ADT
        typeToCheck.adtSig.variants.forEach({ variant ->
            if (variant is TupleVariant) {
                // Find the correct type constructor for this parameterized variant
                val typeConstructor = variant.getTypeConstructorWithParams(typeToCheck.typeParams)

                val variantPatterns = patterns.mapNotNull { (pat, rest) -> 
                    val firstPattern = pat[0]
                    if (firstPattern is TupleTypeConstructorNode &&
                            firstPattern.adtVariant == variant) {
                        Pair(firstPattern.actualArgs, listOf(pat.drop(1)) + rest)
                    } else {
                        null
                    }
                }

                exhaustiveMatches(typeConstructor, variantPatterns, listOf(nextTypes) + restOfTypes,
                        listOf(vars) + varPatterns, trace + TupleVariantBeginTraceOption(variant))
            } else if (variant is RecordVariant) {
                // Find the correct fields for this parameterized variant
                val fields = variant.getFieldsWithParams(typeToCheck.typeParams)

                // Find all fields that appear in the given list of patterns
                val fieldsInPatterns: MutableSet<String> = mutableSetOf()
                patterns.forEach { (pat, _) ->
                    val firstPattern = pat[0]
                    if (firstPattern is RecordTypeConstructorNode &&
                            firstPattern.adtVariant == variant) {
                        fieldsInPatterns.addAll(firstPattern.fields.keys)
                    }
                }

                // Create list of all fields found in patterns along with their types, in order
                val fieldsInOrder: MutableList<String> = mutableListOf()
                val typesInOrder: MutableList<Type> = mutableListOf()

                for (field in fieldsInPatterns) {
                    fieldsInOrder.add(field)
                    typesInOrder.add(fields[field]!!)
                }

                val variantPatterns = patterns.mapNotNull { (pat, rest) -> 
                    val firstPattern = pat[0]
                    if (firstPattern is RecordTypeConstructorNode &&
                            firstPattern.adtVariant == variant) {
                        // Construct list of fields in same order, if field is not specified in
                        // pattern then put in special variable node.
                        val patternList = fieldsInOrder.map { field ->
                            val fieldPattern = firstPattern.fields[field]
                            if (fieldPattern != null) {
                                fieldPattern
                            } else {
                                VARIABLE_NODE
                            }
                        }

                        Pair(patternList, listOf(pat.drop(1)) + rest)
                    } else {
                        null
                    }
                }

                exhaustiveMatches(typesInOrder, variantPatterns, listOf(nextTypes) + restOfTypes,
                        listOf(vars) + varPatterns,
                        trace + RecordVariantBeginTraceOption(variant, fieldsInOrder))
            }
        })
    // Byte type can only be exhaustively matched by wildcard, but gather all literals for trace
    } else if (typeToCheck is ByteType) {
        val vars = filterPatterns(patterns) { _ -> false }

        // Gather all byte literals for trace
        val bytes = patterns.mapNotNull({ (pat, _) ->
            val firstPattern = pat[0]
            if (firstPattern is ByteLiteralNode) {
                firstPattern.num
            } else {
                null
            }
        }).toSet()

        exhaustiveMatches(nextTypes, vars, restOfTypes, varPatterns, trace + ByteTraceOption(bytes))
    // Int type can only be exhaustively matched by wildcard, but gather all literals for trace
    } else if (typeToCheck is IntType) {
        val vars = filterPatterns(patterns) { _ -> false }

        // Gather all int literals for trace
        val ints = patterns.mapNotNull({ (pat, _) ->
            val firstPattern = pat[0]
            if (firstPattern is IntLiteralNode) {
                firstPattern.num
            } else {
                null
            }
        }).toSet()

        exhaustiveMatches(nextTypes, vars, restOfTypes, varPatterns, trace + IntTraceOption(ints))
    // Float type can only be exhaustively matched by wildcard, but gather all literals for trace
    } else if (typeToCheck is FloatType) {
        val vars = filterPatterns(patterns) { _ -> false }

        // Gather all float literals for trace
        val floats = patterns.mapNotNull({ (pat, _) ->
            val firstPattern = pat[0]
            if (firstPattern is FloatLiteralNode) {
                firstPattern.num
            } else {
                null
            }
        }).toSet()

        exhaustiveMatches(nextTypes, vars, restOfTypes, varPatterns,
                trace + FloatTraceOption(floats))
    // Double type can only be exhaustively matched by wildcard, but gather all literals for trace
    } else if (typeToCheck is DoubleType) {
        val vars = filterPatterns(patterns) { _ -> false }

        // Gather all double literals for trace
        val doubles = patterns.mapNotNull({ (pat, _) ->
            val firstPattern = pat[0]
            if (firstPattern is DoubleLiteralNode) {
                firstPattern.num
            } else {
                null
            }
        }).toSet()

        exhaustiveMatches(nextTypes, vars, restOfTypes, varPatterns,
                trace + DoubleTraceOption(doubles))
    // String type can only be exhaustively matched by wildcard, but gather all literals for trace
    } else if (typeToCheck is StringType) {
        val vars = filterPatterns(patterns) { _ -> false }

        // Gather all string literals for trace
        val strings = patterns.mapNotNull({ (pat, _) ->
            val firstPattern = pat[0]
            if (firstPattern is StringLiteralNode) {
                firstPattern.str
            } else {
                null
            }
        }).toSet()

        exhaustiveMatches(nextTypes, vars, restOfTypes, varPatterns,
                trace + StringTraceOption(strings))
    // Vectors can only be exhaustively matched by wildcard, but gather all literal sizes for trace
    } else if (typeToCheck is VectorType) {
        val vars = filterPatterns(patterns) { _ -> false }

        // Gather all vector literal sizes for trace
        val vectorSizes = patterns.mapNotNull({ (pat, _) ->
            val firstPattern = pat[0]
            if (firstPattern is VectorLiteralNode) {
                firstPattern.elements.size
            } else {
                null
            }
        }).toSet()

        exhaustiveMatches(nextTypes, vars, restOfTypes, varPatterns,
                trace + VectorTraceOption(vectorSizes))
    // Set type can only be exhaustively matched by wildcard, but gather all literals sizes for trace
    } else if (typeToCheck is SetType) {
        val vars = filterPatterns(patterns) { _ -> false }

        // Gather all set literal sizes for trace
        val setSizes = patterns.mapNotNull({ (pat, _) ->
            val firstPattern = pat[0]
            if (firstPattern is SetLiteralNode) {
                firstPattern.elements.size
            } else {
                null
            }
        }).toSet()

        exhaustiveMatches(nextTypes, vars, restOfTypes, varPatterns,
                trace + SetTraceOption(setSizes))
    // Map type can only be exhaustively matched by wildcard, but gather all literals sizes for trace
    } else if (typeToCheck is MapType) {
        val vars = filterPatterns(patterns) { _ -> false }

        // Gather all map literal sizes for trace
        val mapSizes = patterns.mapNotNull({ (pat, _) ->
            val firstPattern = pat[0]
            if (firstPattern is MapLiteralNode) {
                firstPattern.keys.size
            } else {
                null
            }
        }).toSet()

        exhaustiveMatches(nextTypes, vars, restOfTypes, varPatterns,
                trace + MapTraceOption(mapSizes))
    // All other types can only be exhaustively matched by variables
    } else {
        val vars = filterPatterns(patterns) { _ -> false }
        exhaustiveMatches(nextTypes, vars, restOfTypes, varPatterns, trace + VariableTraceOption)
    }
}

/**
 * A specific example of a matched case.
 */
sealed class MatchedCase() {
    open fun simplify(): MatchedCase = this
}

object WildcardMatchedCase : MatchedCase() {
    override fun toString(): String = "_"
}

object UnitMatchedCase : MatchedCase() {
    override fun toString(): String = "()"
}

/**
 * A matched case that matches a single literal of any type.
 */
class LiteralMatchedCase<T>(val literal: T) : MatchedCase() {
    override fun toString(): String = literal.toString()
}

/**
 * A matched case that matches a vector of a particular size, with all wildcards.
 */
class WildcardVectorMatchedCase(val size: Int) : MatchedCase() {
    override fun toString(): String {
        // Create list of wildcards of given size
        val wildcard = WildcardMatchedCase.toString()
        val elements = (0 until size).map { _ -> wildcard }

        return elements.joinToString(", ", "[", "]")
    }
}

/**
 * A matched case that matches a set of a particular size, with all wildcards.
 */
class WildcardSetMatchedCase(val size: Int) : MatchedCase() {
    override fun toString(): String {
        // Create list of wildcards of given size
        val wildcard = WildcardMatchedCase.toString()
        val elements = (0 until size).map { _ -> wildcard }

        return elements.joinToString(", ", "{|", "|}")
    }
}

/**
 * A matched case that matches a map of a particular size, with all wildcards.
 */
class WildcardMapMatchedCase(val size: Int) : MatchedCase() {
    override fun toString(): String {
        // Create list of wildcards of given size
        val wildcard = WildcardMatchedCase.toString()
        val elements = (0 until size).map { _ -> wildcard }

        val builder = StringBuilder()
        builder.append("[|")

        var firstPair = true
        for ((key, value) in elements.zip(elements)) {
            // Add a comma separator between every two kvpairs
            if (firstPair) {
                firstPair = false
            } else {
                builder.append(", ")
            }

            // Add the pair itself to the string, formatted as key -> value
            builder.append(key)
            builder.append(" -> ")
            builder.append(value)
        }

        builder.append("|]")

        return builder.toString()
    }
}

/**
 * A matched case that contains nested matched cases.
 *
 * @param elements the nested matched cases
 * @param variant the ADT variant for this matched case, or null if this is a tuple
 * @param fields the list of fields that were checked, in order. This is only non-null if this is
 *        a record variant.
 */
class NestedMatchedCase(
    val elements: MutableList<MatchedCase>,
    val variant: AlgebraicDataTypeVariant?,
    val fields: List<String>?
) : MatchedCase() {
    override fun simplify(): MatchedCase {
        val simplifiedElements = elements.map(MatchedCase::simplify)

        // This nested case can be removed if it is a tuple and all fields are wildcards
        if (simplifiedElements.all { element -> element is WildcardMatchedCase } &&
                variant == null) {
            return WildcardMatchedCase
        } else {
            return NestedMatchedCase(simplifiedElements.toMutableList(), variant, fields)
        }
    }

    override fun toString(): String {
        // If a tuple, format elements into comma separated list
        if (variant is TupleVariant) {
            if (elements.isEmpty()) {
                return variant.name
            } else {
                return variant.name + elements.joinToString(", ", "(", ")")
            }
        // If a record, use elements corresponding to record fields, and all other fields
        // are mapped to wildcards.
        } else if (variant is RecordVariant) {
            val recordExample: MutableMap<String, MatchedCase> = mutableMapOf()
            for ((field, _) in variant.fields) {
                val fieldIdx = fields?.indexOf(field)!!
                if (fieldIdx == -1) {
                    recordExample[field] = WildcardMatchedCase
                } else {
                    recordExample[field] = elements[fieldIdx]
                }
            }

            val fieldString = recordExample.map({ (fieldName, fieldValue) ->
                "${fieldName}: ${fieldValue}"
            }).joinToString(", ", "{ ", " }")
            return "${variant.name} ${fieldString}"
        // Otherwise this must be a tuple
        } else {
            return elements.joinToString(", ", "(", ")")
        }
    }
}

/**
 * Given an exhaustive match checking trace and the corresponding type, construct a case that is
 * unmatched by the corresponding match statement.
 */
private fun traceToMatchedCase(trace: MatchTrace, type: Type): MatchedCase {
    val typeStack: MutableList<MutableList<Type>> = mutableListOf(mutableListOf(type))
    val matchedCase =  NestedMatchedCase(mutableListOf(), null, null)
    val matchedCaseStack: MutableList<NestedMatchedCase> = mutableListOf(matchedCase)

    for (traceOption in trace) {
        when (traceOption) {
            // Variables are simply added as a wildcard match
            is VariableTraceOption -> {
                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(WildcardMatchedCase)
            }
            // Unit literals are simply added as unit matchess
            is UnitTraceOption -> {
                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(UnitMatchedCase)
            }
            // Tuples are added as a new nested match case pushed onto the stack, and types for the
            // tuple are pushed onto the type stack.
            is TupleBeginTraceOption -> {
                val tupleType = typeStack[0].removeAt(0) as TupleType
                typeStack.add(0, tupleType.elementTypes.toMutableList())

                val tupleMatchCase = NestedMatchedCase(mutableListOf(), null, null)
                matchedCaseStack[0].elements.add(tupleMatchCase)
                matchedCaseStack.add(0, tupleMatchCase)
            }
            // When a nested level ends, pop off the type stack and matched case stack
            is EndNestingLevelTraceOption -> {
                val elementTypesLeft = typeStack.removeAt(0)

                // Add wildcards for every type without a corresponding trace, for when a nested
                // case is exited early due to short circuiting.
                for (elementType in elementTypesLeft) {
                    matchedCaseStack[0].elements.add(WildcardMatchedCase)
                }

                matchedCaseStack.removeAt(0)
            }
            // Tuple variants are added as a new nested match case pushed onto the stack, and types
            // for the variant (if they exist) are pushed onto the type stack.
            is TupleVariantBeginTraceOption -> {
                val adtType = typeStack[0].removeAt(0) as AlgebraicDataType
                val typeArgs = traceOption.variant
                        .getTypeConstructorWithParams(adtType.typeParams)

                val variantNestedCase = NestedMatchedCase(mutableListOf(),
                        traceOption.variant, null)
                matchedCaseStack[0].elements.add(variantNestedCase)

                // Push onto the type stack (and matched case stack) if variant has type args 
                if (!typeArgs.isEmpty()) {
                    typeStack.add(0, typeArgs.toMutableList())
                    matchedCaseStack.add(0, variantNestedCase)
                }
            }
            // Tuple variants are added as a new nested match case pushed onto the stack, and types
            // for the variant (if they exist) are pushed onto the type stack.
            is RecordVariantBeginTraceOption -> {
                val adtType = typeStack[0].removeAt(0) as AlgebraicDataType
                val fields = traceOption.variant.getFieldsWithParams(adtType.typeParams)

                // Construct list of type corresponding to matched fields, in same orde as fields
                val typesInOrder: MutableList<Type> = mutableListOf()
                for (field in traceOption.fields) {
                    typesInOrder.add(fields[field]!!)
                }

                val variantNestedCase = NestedMatchedCase(mutableListOf(),
                        traceOption.variant, traceOption.fields)
                matchedCaseStack[0].elements.add(variantNestedCase)

                // Push onto the type stack (and matched case stack) if variant has type args 
                if (!typesInOrder.isEmpty()) {
                    typeStack.add(0, typesInOrder.toMutableList())
                    matchedCaseStack.add(0, variantNestedCase)
                }
            }
            // Bools are simply added as bool matches
            is BoolTraceOption -> {
                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(LiteralMatchedCase(traceOption.bool))
            }
            // Bytes are added with an example byte that is the smallest whole number not matched
            is ByteTraceOption -> {
                val exampleByte = findSmallestUnmatchedWholeNumber(
                        traceOption.bytes.map(Byte::toInt).toSet()).toByte()

                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(LiteralMatchedCase(exampleByte))
            }
            // Ints are added with an example int that is the smallest whole number not matched
            is IntTraceOption -> {
                val exampleInt = findSmallestUnmatchedWholeNumber(traceOption.ints)

                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(LiteralMatchedCase(exampleInt))
            }
            // Floats are added with an example float that is the smallest whole number not matched
            is FloatTraceOption -> {
                // Find all floats that are equal to integers
                val matchedWholeFloats = traceOption.floats.filter({ f -> f - f.toInt() == 0.0f })
                        .map(Float::toInt).toSet()
                val exampleFloat = findSmallestUnmatchedWholeNumber(matchedWholeFloats).toFloat()

                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(LiteralMatchedCase(exampleFloat))
            }
            // Doubles are added with an example double that is the smallest unmatched whole number
            is DoubleTraceOption -> {
                // Find all doubles that are equal to integers
                val matchedWholeDoubles = traceOption.doubles.filter({ f -> f - f.toInt() == 0.0 })
                        .map(Double::toInt).toSet()
                val exampleDouble = findSmallestUnmatchedWholeNumber(matchedWholeDoubles).toDouble()

                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(LiteralMatchedCase(exampleDouble))
            }
            // Strings are added with an example string that is the first (lexicographically)
            // string that is not matched.
            is StringTraceOption -> {
                val exampleString = findFirstUnmatchedString(traceOption.strings)

                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(LiteralMatchedCase(exampleString))
            }
            // Vectors are added with an example vector that contains wildcards and is the smallest
            // size of vector not matched.
            is VectorTraceOption -> {
                val exampleVectorSize = findSmallestUnmatchedWholeNumber(traceOption.vectorSizes)

                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(WildcardVectorMatchedCase(exampleVectorSize))
            }
            // Set are added with an example set that contains wildcards and is the smallest
            // size of set not matched.
            is SetTraceOption -> {
                val exampleSetSize = findSmallestUnmatchedWholeNumber(traceOption.setSizes)

                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(WildcardSetMatchedCase(exampleSetSize))
            }
            // Maps are added with an example map that contains wildcards and is the smallest
            // size of map not matched.
            is MapTraceOption -> {
                val exampleMapSize = findSmallestUnmatchedWholeNumber(traceOption.mapSizes)

                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(WildcardMapMatchedCase(exampleMapSize))
            }
        }
    }

    // Ignore the last element in the type stack, it is a necessary artifact
    typeStack.removeAt(typeStack.size - 1)

    // Add wildcards for every type left in the type stack without a trace, caused by exiting early
    for (typeList in typeStack) {
        for (curType in typeList) {
            matchedCaseStack[0].elements.add(WildcardMatchedCase)
        }

        matchedCaseStack.removeAt(0)
    }

    // Simply and return the lone matched case stored within the root matched case
    return matchedCase.elements[0].simplify()
}

/**
 * Find the smallest whole number (an integer >= 0) that does not appear in the provided set.
 */
private fun findSmallestUnmatchedWholeNumber(ints: Set<Int>): Int {
    for (i in 0 until ints.size) {
        if (!ints.contains(i)) {
            return i
        }
    }

    return ints.size
}

/**
 * Return an int i to the i'th lowercase string in lexicographic order.
 */
private fun mapIntToLexicographicString(i: Int): String {
    val builder = StringBuilder()
    
    var currentInt = i
    
    while (currentInt > 0) {
        val c = currentInt % 26
        
        if (currentInt >= 26) {
            builder.append('a'.plus(c))
        } else {
            builder.append('a'.plus(c - 1))
        }
        
        currentInt = currentInt / 26
    }

    return builder.reverse().toString()
}

/**
 * Find the lexicographically first (alphanbetic, lowercase) string that does not appear in the
 * provided set.
 */
private fun findFirstUnmatchedString(strings: Set<String>): String {
    for (i in 0 until strings.size) {
        val string = mapIntToLexicographicString(i)
        if (!strings.contains(string)) {
            return "\"" + string + "\""
        }
    }

    return "\"" + mapIntToLexicographicString(strings.size) + "\""
}

