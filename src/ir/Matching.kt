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

// An int type, along with all int literals that were checked
class IntTraceOption(val ints: Set<Int>) : MatchTraceOption()

// A float type, along with all float literals that were checked
class FloatTraceOption(val floats: Set<Double>) : MatchTraceOption()

// An ADR, along with the variant that was checked
class VariantBeginTraceOption(val variant: AlgebraicDataTypeVariant) : MatchTraceOption()

class InexhaustiveMatchException(val trace: MatchTrace) : Exception()

/**
 * Check whether every match statement in the tree rooted at the given root has exhaustive match
 * cases for its type.
 */
fun exhaustiveMatchCases(root: IRNode) {
    root.map { node -> 
        if (node is MatchNode) {
            val patterns = node.cases.map { (pattern, _) ->
                    Pair<List<IRNode>, List<List<IRNode>>>(listOf(pattern), listOf())
            }

            try {
                exhaustiveMatches(listOf(node.expr.type), patterns, listOf(), listOf(), listOf())
            } catch (e: InexhaustiveMatchException) {
                val trace = traceToMatchedCase(e.trace, node.expr.type)
                throw IRConversionException("Inexhaustive match cases. Here is an example of a " +
                        "case that is not matched: ${trace}", node.startLocation)
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
fun filterPatterns(
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
fun exhaustiveMatches(
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
            // Find the correct type constructor for this parameterized variant
            val typeConstructor = variant.getTypeConstructorWithParams(typeToCheck.typeParams)

            val variantPatterns = patterns.mapNotNull { (pat, rest) -> 
                val firstPattern = pat[0]
                if (firstPattern is TypeConstructorNode && firstPattern.adtVariant == variant) {
                    Pair(firstPattern.actualArgs, listOf(pat.drop(1)) + rest)
                } else {
                    null
                }
            }

            exhaustiveMatches(typeConstructor, variantPatterns, listOf(nextTypes) + restOfTypes,
                    listOf(vars) + varPatterns, trace + VariantBeginTraceOption(variant))
        })
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
 * A matched case that contains nested matched cases.
 *
 * @param elements the nested matched cases
 * @param name the name of this case. If null, this is a tuple. If non-null, this is an ADT variant
 *        with that name
 */
class NestedMatchedCase(val elements: MutableList<MatchedCase>, val name: String?) : MatchedCase() {
    override fun simplify(): MatchedCase {
        val simplifiedElements = elements.map(MatchedCase::simplify)

        // This nested case can be removed if it is a tuple and all fields are wildcards
        if (name == null && simplifiedElements.all { element -> element is WildcardMatchedCase }) {
            return WildcardMatchedCase
        } else {
            return NestedMatchedCase(simplifiedElements.toMutableList(), name)
        }
    }

    override fun toString(): String {
        if (name == null) {
            return elements.joinToString(", ", "(", ")")
        } else if (elements.isEmpty()) {
            return name
        } else {
            return name + elements.joinToString(", ", "(", ")")
        }
    }
}

/**
 * Given an exhaustive match checking trace and the corresponding type, construct a case that is
 * unmatched by the corresponding match statement.
 */
fun traceToMatchedCase(trace: MatchTrace, type: Type): MatchedCase {
    val typeStack: MutableList<MutableList<Type>> = mutableListOf(mutableListOf(type))
    val matchedCase =  NestedMatchedCase(mutableListOf(), null)
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

                val tupleMatchCase = NestedMatchedCase(mutableListOf(), null)
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
            // Bools are simply added as bool matches
            is BoolTraceOption -> {
                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(LiteralMatchedCase(traceOption.bool))
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
                val matchedWholeFloats = traceOption.floats.filter({ f -> f - f.toInt() == 0.0 })
                        .map(Double::toInt).toSet()
                val exampleFloat = findSmallestUnmatchedWholeNumber(matchedWholeFloats).toDouble()

                typeStack[0].removeAt(0)
                matchedCaseStack[0].elements.add(LiteralMatchedCase(exampleFloat))
            }
            // Variants are added as a new nested match case pushed onto the stack, and types for
            // the variant (if they exist) are pushed onto the type stack.
            is VariantBeginTraceOption -> {
                val adtType = typeStack[0].removeAt(0) as AlgebraicDataType
                val typeArgs = traceOption.variant.getTypeConstructorWithParams(adtType.typeParams)

                val variantNestedCase = NestedMatchedCase(mutableListOf(), traceOption.variant.name)
                matchedCaseStack[0].elements.add(variantNestedCase)

                // Push onto the type stack (and matched case stack) if variant has type arguments 
                if (!typeArgs.isEmpty()) {
                    typeStack.add(0, typeArgs.toMutableList())
                    matchedCaseStack.add(0, variantNestedCase)
                }
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
fun findSmallestUnmatchedWholeNumber(ints: Set<Int>): Int {
    val matchedInts = ints.toMutableList()
    matchedInts.sort()

    // Find the index of 0 in the sorted list
    val zeroIndex = matchedInts.indexOf(0)
    if (zeroIndex == -1) {
        // If zero does not exist, it is the smallest whole number
        return 0
    } else {
        // Otherwise traverse list from index containing 0, finding smallest integer not included
        var i = 1
        var ind = zeroIndex + 1

        while (ind < matchedInts.size && matchedInts[ind] == i) {
            i += 1
            ind += 1
        }

        return i
    }
}
