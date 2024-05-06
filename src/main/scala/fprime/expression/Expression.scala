package fprime.expression

import fprime.parser.combinators.*
import fprime.parser.{ParseError, Parser}
import fprime.parsing.{Parsable, Tokens}

import scala.util.Failure

sealed trait Expression

case class Variable(var symbol: Symbol) extends Expression

case class Abstraction[P <: Expression, B <: Expression](var parameter: P, var body: B)
    extends Expression

case class Application[C <: Expression, A <: Expression](var callable: C, var argument: A)
    extends Expression

given [T <: Expression, E <: Expression](using
    downcast: Conversion[E, T & E],
    expression: Parsable[E],
): Parsable[T] with
    override lazy val parser: Parser[Tokens, T] = expression.parser.map(identity)

given (using symbol: Parsable[Symbol]): Parsable[Variable] with
    override lazy val parser: Parser[Tokens, Variable] = symbol.parser.map(s => Variable(s))

given [P <: Expression, B <: Expression](using
    parameter: Parsable[P],
    body: Parsable[B],
    downcast: Conversion[Abstraction[P, B], B & Abstraction[P, B]], // shape preserving
): Parsable[Abstraction[P, B]] with
    private val parameters = parameter.parser.atLeast(1).thenSkip(Literal.parser("."))
    private val lambda =
        Literal.parser("λ") `orElse` Literal.parser("@") `orElse` Literal.parser("\\")

    override lazy val parser: Parser[Tokens, Abstraction[P, B]] =
        lambda
            .skipThen(parameters)
            .andThen(body.parser)
            .map((parameters, body) =>
                parameters
                    .foldRight(body)((parameter: P, body: B) => Abstraction(parameter, body): B)
                    .asInstanceOf[Abstraction[P, B]]
                // downcast safety: cannot fail because we've upcasted in the last folding step
            )

given [C <: Expression, A <: Expression](using
    callable: Parsable[C],
    argument: Parsable[A],
    downcast: Conversion[Application[C, A], C & Application[C, A]], // shape preserving
): Parsable[Application[C, A]] with
    private val parens = this.parser.between(Literal.parser("("), Literal.parser(")"))

    override lazy val parser: Parser[Tokens, Application[C, A]] =
        callable.parser
            .andThen(argument.parser.atLeast(1))
            .map((head, tail) =>
                tail.foldLeft(head)((callable, argument) => Application(callable, argument): C)
                    .asInstanceOf[Application[C, A]]
            )

given (using
    variable: Parsable[Variable],
    abstraction: Parsable[Abstraction[?, ?]],
    application: Parsable[Application[?, ?]],
): Parsable[Expression] with
    import scala.reflect.ClassTag

    private val parens = this.parser.between(Literal.parser("("), Literal.parser(")"))
    private var pending: Option[ClassTag[?]] = None

    extension [T](self: Parser[Tokens, T])
        private def nonRecur(using tag: ClassTag[T]): Parser[Tokens, T] = (input: Tokens) =>
            if pending.isDefined && pending.get.equals(tag) then
                Failure(ParseError(input, "Left-recursion detected."))
            else
                val old = pending
                pending = Some(tag)
                val result = self.parse(input)
                pending = old
                result

    override lazy val parser: Parser[Tokens, Expression] =
        abstraction.parser.nonRecur
            .orElse(application.parser.nonRecur)
            .orElse(variable.parser.nonRecur)
            .orElse(parens.nonRecur)
