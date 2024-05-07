package fprime.traverse

import fprime.expression.{Abstraction, Application, Expression, Variable}

object DeBruijnSubstitution:
    private def traverse(
        expression: Expression,
        target: Int,
        replacement: Expression,
    ): Expression =
        expression match
            case variable @ Variable(_) =>
                variable match
                    case deBruijn: DeBruijnVariable if deBruijn.index == target => replacement
                    case _                                                      => variable

            case abstraction @ Abstraction(parameter, body) =>
                DeBruijnShifter.shift(replacement, 1)
                val b = traverse(abstraction.body, target + 1, replacement)
                abstraction.copy(body = b)

            case application @ Application(callable, argument) =>
                val c = traverse(application.callable, target, replacement)
                val a = traverse(application.argument, target, replacement)
                application.copy(callable = c, argument = a)

    def substitute[E <: Expression, R <: E](
        expression: E,
        target: Int,
        replacement: R,
    ): E =
        traverse(expression, target, replacement).asInstanceOf[E]
