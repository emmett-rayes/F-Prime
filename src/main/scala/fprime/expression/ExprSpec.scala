package fprime.expression

import fprime.parser.{Parser, given}
import fprime.parsing.{Parsable, Tokens}

/* Expression Specialization */
trait ExprSpec extends Product1[Expression]

given [T <: ExprSpec]: Conversion[T, Expression] = _._1

given [T <: ExprSpec, E <: Expression](using
    downcast: Conversion[E, T],
    expression: Parsable[E],
): Parsable[T] with
    override lazy val parser: Parser[Tokens, T] = expression.parser
