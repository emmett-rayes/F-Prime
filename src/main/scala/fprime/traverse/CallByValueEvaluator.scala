package fprime.traverse

import fprime.eval.{BetaReduction, TracingBetaReduction}
import fprime.expression.{Abstraction, Application, Expression, Variable}

import scala.PartialFunction.cond
import scala.util.control.Breaks.*

object CallByValueEvaluator extends BetaReduction with TracingBetaReduction:
    override def reduceOnce[E <: Expression](
        expression: E,
        normalize: Boolean = false,
    ): Option[E] =
        given Boolean = normalize
        traverse(expression).map(x => x.asInstanceOf[E])

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
