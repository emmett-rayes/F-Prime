package fprime.traverse

import fprime.expression.{Abstraction, Application, Expression, Variable}

object DeBruijnSubstitution:
    // TODO: find a better solution than casting
    def substitute[E <: Expression, R <: E](expression: E, target: Int, replacement: R): E =
        expression match
            case variable @ Variable(_) =>
                variable match
                    case deBruijn: DeBruijnVariable if deBruijn.index == target => replacement
                    case _                                                      => variable

            case abstraction @ Abstraction(parameter, body) =>
                val shifted = DeBruijnShifter.shift(replacement, 1)
                abstraction.body =
                    substitute(abstraction.body, target + 1, shifted.asInstanceOf[body.type])
                abstraction

            case application @ Application(callable, argument) =>
                application.callable = substitute(
                  application.callable,
                  target,
                  replacement.asInstanceOf[callable.type],
                )
                application.argument = substitute(
                  application.argument,
                  target,
                  replacement.asInstanceOf[argument.type],
                )
                application
