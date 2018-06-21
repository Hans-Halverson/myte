package myte.parser.ast

import myte.shared.*

sealed class TypeExpression()

object UnitTypeExpression : TypeExpression()

object BoolTypeExpression : TypeExpression()

object ByteTypeExpression : TypeExpression()

object IntTypeExpression : TypeExpression()

object FloatTypeExpression : TypeExpression()

object DoubleTypeExpression : TypeExpression()

object StringTypeExpression : TypeExpression()

class VectorTypeExpression(
    val elementType: TypeExpression
) : TypeExpression()

class SetTypeExpression(
    val elementType: TypeExpression
) : TypeExpression()

class MapTypeExpression(
    val keyType: TypeExpression,
    val valType: TypeExpression
) : TypeExpression()

class TupleTypeExpression(
    val elementTypes: List<TypeExpression>
) : TypeExpression()

class FunctionTypeExpression(
    val argTypes: List<TypeExpression>,
    val returnType: TypeExpression
) : TypeExpression()

class VariableTypeExpression(
    val ident: ResolvableSymbol,
    val typeParams: List<TypeExpression>,
    val startLocation: Location
) : TypeExpression()
