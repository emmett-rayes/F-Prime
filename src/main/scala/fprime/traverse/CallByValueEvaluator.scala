package fprime.traverse

import fprime.expression.{Abstraction, Application, Expression, Variable}

import scala.PartialFunction.cond
import scala.util.control.Breaks.*

trait BetaReduction:
    def reduceOnce[E <: Expression & R, R <: Expression](
        expression: E,
        normalize: Boolean = false,
    ): Option[R]

    def reduce[E <: Expression & R, R <: Expression](
        expression: E,
        normalize: Boolean = false,
    ): Option[R] =
        var reduced = false
        var result = expression
        while true do
            reduceOnce(result, normalize) match
                case None => return if reduced then Some(result) else None
                case Some(r) =>
                    reduced = true
                    result = r
        throw RuntimeException("unreachable!")

object CallByValueEvaluator extends BetaReduction:
    override def reduceOnce[E <: Expression & R, R <: Expression](
        expression: E,
        normalize: Boolean = false,
    ): Option[R] =
        given Boolean = normalize
        traverse(expression).map(x => x.asInstanceOf[R])

    private def traverse(expression: Expression)(using normalize: Boolean): Option[Expression] =
        expression match
            case _: Variable =>
                None

            case abstraction @ Abstraction(_, body) =>
                if !normalize then None
                else
                    traverse(body) match
                        case None    => None
                        case Some(b) => Some(abstraction.copy(body = b))

            case application @ Application(callable, argument) =>
                if !normalize && callable.isValue then return None
                else
                    traverse(callable) match
                        case None    =>
                        case Some(c) => return Some(application.copy(callable = c))

                if !normalize && argument.isValue then return None
                else
                    traverse(argument) match
                        case None    =>
                        case Some(a) => return Some(application.copy(argument = a))

                callable match
                    case Abstraction(parameter, body) =>
                        val shiftedArgument = DeBruijnShifter.shift(argument, 1)
                        val substituted =
                            DeBruijnSubstitution.substitute(body, target = 1, shiftedArgument)
                        val shiftedBody = DeBruijnShifter.shift(substituted, -1)
                        Some(shiftedBody)
                    case _ => None

extension (expression: Expression)
    def isValue: Boolean = cond(expression) { case _: Abstraction[?, ?] => true }
