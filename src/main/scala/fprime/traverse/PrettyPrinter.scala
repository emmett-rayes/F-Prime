package fprime.traverse

import fprime.expression.{Abstraction, Application, Expression, Variable}
import fprime.traverse.DeBruijnVariable

import scala.PartialFunction.cond

object PrettyPrinter:
    enum Mode:
        case Named
        case Indexed
        case NamelessLocals

    private def pretty(expression: Expression, scope: Int)(using mode: Mode): String =
        expression match
            case variable @ Variable(symbol) =>
                mode match
                    case Mode.Named => variable.symbol
                    case _ =>
                        variable match
                            case deBruijn: DeBruijnVariable =>
                                mode match
                                    case Mode.Named   => throw RuntimeException("unreachable!")
                                    case Mode.Indexed => deBruijn.index.toString
                                    case Mode.NamelessLocals =>
                                        if deBruijn.index <= scope then deBruijn.index.toString
                                        else variable.symbol
                            case _ => symbol

            case Abstraction(parameter, body) =>
                val bodyIsAbstraction = cond(body) { case Abstraction(_, _) => true }
                val prettyParameter = pretty(parameter, scope)
                var prettyBody = pretty(body, scope)
                if bodyIsAbstraction then
                    prettyBody = prettyBody.stripPrefix("(").stripSuffix(")")
                mode match
                    case Mode.Named => s"(λ$prettyParameter. $prettyBody)"
                    case Mode.Indexed | Mode.NamelessLocals => s"(λ $prettyBody)"

            case Application(callable, argument) =>
                val argumentIsApplication = cond(argument) { case Application(_, _) => true }
                val prettyCallable = pretty(callable, scope)
                var prettyArgument = pretty(argument, scope)
                if argumentIsApplication then prettyArgument = s"($prettyArgument)"
                s"$prettyCallable $prettyArgument"

    def pretty(expression: Expression, mode: Mode = Mode.Named): String =
        given Mode = mode
        val expressionIsAbstraction = cond(expression) { case Abstraction(_, _) => true }
        val string = pretty(expression, 0)
        if expressionIsAbstraction then string.stripPrefix("(").stripSuffix(")") else string
