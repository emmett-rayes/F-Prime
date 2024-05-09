package fprime.traverse

import fprime.eval.BetaReduction
import fprime.expression.*

import scala.PartialFunction.cond

object CallByValueEvaluator extends BetaReduction:
    private def traverse[E <: Expression](expression: E)(using normalize: Boolean): Option[E] =
        expression match
            case _: Variable =>
                None

            case abstraction @ Abstraction(_, body) =>
                if !normalize then None
                else
                    traverse(body) match
                        case None => None
                        case Some(b) =>
                            Some(abstraction.copy(body = b).asInstanceOf[abstraction.type])

            case application @ Application(callable, argument) =>
                if normalize || !callable.isValue then
                    traverse(callable) match
                        case None =>
                        case Some(c) =>
                            return Some(
                              application.copy(callable = c).asInstanceOf[application.type]
                            )

                if normalize || !argument.isValue then
                    traverse(argument) match
                        case None =>
                        case Some(a) =>
                            return Some(
                              application.copy(argument = a).asInstanceOf[application.type]
                            )

                callable match
                    case Abstraction(parameter, body) =>
                        val shiftedArgument = DeBruijnShifter.shift(argument, 1)
                        val substituted =
                            DeBruijnSubstitution.substitute(body, target = 1, shiftedArgument)
                        val shiftedBody = DeBruijnShifter.shift(substituted, -1)
                        // noinspection ScalaRedundantCast
                        Some(shiftedBody.asInstanceOf[E])
                    case _ => None

            case ascription @ Ascription(expression, targetType) =>
                traverse(expression) match
                    case None => None
                    case Some(e) =>
                        Some(ascription.copy(expression = e).asInstanceOf[ascription.type])

    override def reduceOnce[E <: Expression](
        expression: E,
        normalize: Boolean = false,
    ): Option[E] =
        given Boolean = normalize
        traverse(expression)

extension (expression: Expression)
    def isValue: Boolean = cond(expression) { case _: Abstraction[?, ?] => true }
