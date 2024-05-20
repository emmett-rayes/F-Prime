package fprime.eval

import fprime.expression.Expression

import scala.collection.mutable.ArrayBuffer

trait BetaReduction:
    def reduceOnce[E <: Expression](
        expression: E,
        normalize: Boolean = false,
    ): Option[E]

    def reduce[E <: Expression](
        expression: E,
        normalize: Boolean = false,
    ): List[E] =
        val trace = ArrayBuffer[E]()
        var current = expression
        while true do
            trace.append(current)
            reduceOnce(current, normalize) match
                case None          => return trace.toList
                case Some(reduced) => current = reduced
        throw Exception("unreachable!")
