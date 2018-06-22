package myte.eval.builtins

import myte.shared.*

const val OPTION_TYPE_NAME = "Option"
const val OPTION_TYPE_NONE_VARIANT = "None"
const val OPTION_TYPE_SOME_VARIANT = "Some"

const val COMPARISON_TYPE_NAME = "Comparison"
const val COMPARISON_TYPE_EQUAL_VARIANT = "EqualComparison"
const val COMPARISON_TYPE_LESS_VARIANT = "LessComparison"
const val COMPARISON_TYPE_GREATER_VARIANT = "GreaterComparison"

const val ADD_TRAIT_NAME = "Add"
const val SUBTRACT_TRAIT_NAME = "Subtract"
const val MULTIPLY_TRAIT_NAME = "Multiply"
const val DIVIDE_TRAIT_NAME = "Divide"
const val POWER_TRAIT_NAME = "Power"
const val REMAINDER_TRAIT_NAME = "Remainder"
const val UNARY_PLUS_TRAIT_NAME = "UnaryPlus"
const val UNARY_MINUS_TRAIT_NAME = "UnaryMinus"
const val INDEX_TRAIT_NAME = "Index"
const val INDEX_ASSIGN_TRAIT_NAME = "IndexAssign"
const val ITERATOR_TRAIT_NAME = "Iterator"
const val ITERABLE_TRAIT_NAME = "Iterable"
const val COMPARABLE_TRAIT_NAME = "Comparable"
const val EQUAL_TRAIT_NAME = "Equal"

val OPTION_TYPE_SIG = AlgebraicDataTypeSignature(OPTION_TYPE_NAME, listOf(TypeParameter()))
val COMPARISON_TYPE_SIG = AlgebraicDataTypeSignature(COMPARISON_TYPE_NAME, listOf())

val COMPARISON_TYPE = COMPARISON_TYPE_SIG.createTypeWithParams(listOf())

val ADD_TRAIT_SIG = TraitSignature(ADD_TRAIT_NAME, listOf(TypeParameter()))
val SUBTRACT_TRAIT_SIG = TraitSignature(SUBTRACT_TRAIT_NAME, listOf(TypeParameter()))
val MULTIPLY_TRAIT_SIG = TraitSignature(MULTIPLY_TRAIT_NAME, listOf(TypeParameter()))
val DIVIDE_TRAIT_SIG = TraitSignature(DIVIDE_TRAIT_NAME, listOf(TypeParameter()))
val POWER_TRAIT_SIG = TraitSignature(POWER_TRAIT_NAME, listOf(TypeParameter()))
val REMAINDER_TRAIT_SIG = TraitSignature(REMAINDER_TRAIT_NAME, listOf(TypeParameter()))
val UNARY_PLUS_TRAIT_SIG = TraitSignature(UNARY_PLUS_TRAIT_NAME, listOf(TypeParameter()))
val UNARY_MINUS_TRAIT_SIG = TraitSignature(UNARY_MINUS_TRAIT_NAME, listOf(TypeParameter()))
val INDEX_TRAIT_SIG = TraitSignature(INDEX_TRAIT_NAME, listOf(TypeParameter(), TypeParameter()))
val INDEX_ASSIGN_TRAIT_SIG = TraitSignature(INDEX_ASSIGN_TRAIT_NAME,
        listOf(TypeParameter(), TypeParameter()))
val ITERATOR_TRAIT_SIG = TraitSignature(ITERATOR_TRAIT_NAME, listOf(TypeParameter()))
val ITERABLE_TRAIT_SIG = TraitSignature(ITERABLE_TRAIT_NAME, listOf(TypeParameter()))
val COMPARABLE_TRAIT_SIG = TraitSignature(COMPARABLE_TRAIT_NAME, listOf(TypeParameter()))
val EQUAL_TRAIT_SIG = TraitSignature(EQUAL_TRAIT_NAME, listOf(TypeParameter()))

val BUILTIN_TYPES: Map<String, AlgebraicDataTypeSignature> = mapOf(
    OPTION_TYPE_NAME to OPTION_TYPE_SIG,
    COMPARISON_TYPE_NAME to COMPARISON_TYPE_SIG
)

val BUILTIN_TRAITS: Map<String, TraitSignature> = mapOf(
    ADD_TRAIT_NAME to ADD_TRAIT_SIG,
    SUBTRACT_TRAIT_NAME to SUBTRACT_TRAIT_SIG,
    MULTIPLY_TRAIT_NAME to MULTIPLY_TRAIT_SIG,
    DIVIDE_TRAIT_NAME to DIVIDE_TRAIT_SIG,
    POWER_TRAIT_NAME to POWER_TRAIT_SIG,
    REMAINDER_TRAIT_NAME to REMAINDER_TRAIT_SIG,
    UNARY_PLUS_TRAIT_NAME to UNARY_PLUS_TRAIT_SIG,
    UNARY_MINUS_TRAIT_NAME to UNARY_MINUS_TRAIT_SIG,
    INDEX_TRAIT_NAME to INDEX_TRAIT_SIG,
    INDEX_ASSIGN_TRAIT_NAME to INDEX_ASSIGN_TRAIT_SIG,
    ITERATOR_TRAIT_NAME to ITERATOR_TRAIT_SIG,
    ITERABLE_TRAIT_NAME to ITERABLE_TRAIT_SIG,
    COMPARABLE_TRAIT_NAME to COMPARABLE_TRAIT_SIG,
    EQUAL_TRAIT_NAME to EQUAL_TRAIT_SIG
)

fun getVariantForBuiltinType(
    adtSig: AlgebraicDataTypeSignature,
    variantName: String
): AlgebraicDataTypeVariant {
    val variant = adtSig.variants.find { variant -> variant.name == variantName }
    if (variant == null) {
        throw ExceptionWithoutLocation("Variant with name ${variantName} not defined for builtin " +
                "type ${adtSig.name}. Have you included the standard library?")
    }

    return variant
}
