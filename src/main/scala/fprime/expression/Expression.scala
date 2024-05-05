package fprime.expression

import fprime.parser.combinators.*
import fprime.parser.{ParseError, Parser}
import fprime.parsing.{Parsable, Tokens}

import scala.util.Failure

sealed trait Expression
case class Variable(symbol: Symbol) extends Expression
case class Abstraction[P <: ExprSpec, B <: ExprSpec](parameter: P, body: B) extends Expression
case class Application[C <: ExprSpec, A <: ExprSpec](callable: C, argument: A)
    extends Expression

given (using symbol: Parsable[Symbol]): Parsable[Variable] with
    override lazy val parser: Parser[Tokens, Variable] = symbol.parser.map(s => Variable(s))

given [P <: ExprSpec, B <: ExprSpec](using
    parameter: Parsable[P],
    body: Parsable[B],
    upcast: Conversion[Abstraction[P, B], B],
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
                    ._1
                    .asInstanceOf[Abstraction[P, B]]
                // downcast safety: cannot fail because we've upcasted in the last folding step
            )

given [C <: ExprSpec, A <: ExprSpec](using
    callable: Parsable[C],
    argument: Parsable[A],
    upcast: Conversion[Application[C, A], C],
): Parsable[Application[C, A]] with
    private val parens = this.parser.between(Literal.parser("("), Literal.parser(")"))

    override lazy val parser: Parser[Tokens, Application[C, A]] =
        callable.parser
            .andThen(argument.parser.atLeast(1))
            .map((head, tail) =>
                tail.foldLeft(head)((callable, argument) => Application(callable, argument): C)
                    ._1
                    .asInstanceOf[Application[C, A]]
                // downcast safety: cannot fail because we've upcasted in the last folding step
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
                Failure(ParseError("Left-recursion detected."))
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
