package myte.eval.values

import myte.ir.nodes.*
import myte.shared.*

open class ClosureValue(
    val formalArgs: List<Identifier>,
    val body: IRNode,
    val environment: Environment,
    type: FunctionType
) : Value(type) {
    override fun toString(): String = "<function>"
}

class MethodValue(
    formalArgs: List<Identifier>,
    body: IRNode,
    environment: Environment,
    val thisIdent: Identifier,
    val receiver: Value?,
    type: FunctionType
) : ClosureValue(formalArgs, body, environment, type) {
    fun withReceiver(recv: Value, methodType: Type): MethodValue {
        return MethodValue(formalArgs, body, environment, thisIdent, recv,
                methodType as FunctionType)
    }
}
