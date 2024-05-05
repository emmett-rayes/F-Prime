package fprime.traverse

import fprime.expression.{Abstraction, Application, Expression, Variable}

import scala.PartialFunction.cond

object PrettyPrinter:
    def pretty(expression: Expression): String = expression match
        case Variable(symbol) => symbol

        case Abstraction(parameter, body) =>
            val bodyIsAbstraction = cond(body) { case Abstraction(_, _) => true }
            val prettyParameter = pretty(parameter)
            var prettyBody = pretty(body)
            if bodyIsAbstraction then prettyBody = prettyBody.stripPrefix("(").stripSuffix(")")
            s"(λ$prettyParameter. $prettyBody)"

        case Application(callable, argument) =>
            val argumentIsApplication = cond(argument) { case Application(_, _) => true }
            val prettyCallable = pretty(callable)
            var prettyArgument = pretty(argument)
            if argumentIsApplication then prettyArgument = s"($prettyArgument)"
            s"$prettyCallable $prettyArgument"
