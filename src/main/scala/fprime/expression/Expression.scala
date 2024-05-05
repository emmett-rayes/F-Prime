package fprime.expression

import fprime.parser.combinators.*
import fprime.parser.{ParseError, Parser}
import fprime.parsing.{Parsable, Tokens}

import scala.util.Failure

sealed trait Expression

class Variable(val symbol: Symbol) extends Expression

object Variable:
    def unapply(variable: Variable): Tuple1[Symbol] = Tuple1(variable.symbol)

class Abstraction[P <: Expression, B <: Expression](val parameter: P, val body: B)
    extends Expression

object Abstraction:
    def unapply[P <: Expression, B <: Expression](abstraction: Abstraction[P, B]): (P, B) =
        (abstraction.parameter, abstraction.body)

class Application[C <: Expression, A <: Expression](val callable: C, val argument: A)
    extends Expression

object Application:
    def unapply[C <: Expression, A <: Expression](application: Application[C, A]): (C, A) =
        (application.callable, application.argument)

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
