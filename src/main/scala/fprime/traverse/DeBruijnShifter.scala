package fprime.traverse

import fprime.expression.{Abstraction, Application, Expression, Variable}

object DeBruijnShifter:
    private def shift[E <: Expression](expression: E, place: Int, cutoff: Int): E =
        expression match
            case variable @ Variable(_) =>
                variable match
                    case deBruijn: DeBruijnVariable =>
                        if deBruijn.index >= cutoff then deBruijn.index += place
                        deBruijn
                    case _ => variable

            case abstraction @ Abstraction(parameter, body) =>
                abstraction.body = shift(body, place, cutoff + 1)
                abstraction

            case application @ Application(callable, argument) =>
                application.callable = shift(callable, place, cutoff)
                application.argument = shift(argument, place, cutoff)
                application

    def shift[E <: Expression](expression: E, place: Int): E = shift(expression, place, 1)
