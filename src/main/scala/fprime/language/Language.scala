package fprime.language

import fprime.expression.Expression
import fprime.expression.Parsing.{Parsable, Tokens}
import fprime.parser.{Parser, given}

trait Language extends Product1[Expression]

given [T <: Language]: Conversion[T, Expression] = _._1

given [T <: Language, E <: Expression](using
    downcast: Conversion[E, T],
    expression: Parsable[E],
): Parsable[T] with
    override lazy val parser: Parser[Tokens, T] = expression.parser
