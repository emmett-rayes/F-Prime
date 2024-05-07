package fprime.eval

import fprime.expression.Expression

trait BetaReduction:
    def reduceOnce[E <: Expression & R, R <: Expression](
        expression: E,
        normalize: Boolean = false,
    ): Option[R]

    def reduce[E <: Expression & R, R <: Expression](
        expression: E,
        normalize: Boolean = false,
    ): Option[R] =
        var strict = false
        var current = expression
        while true do
            reduceOnce(current, normalize) match
                case None => return if strict then Some(current) else None
                case Some(reduced) =>
                    strict = true
                    current = reduced
        throw RuntimeException("unreachable!")
