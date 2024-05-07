package fprime.traverse

import fprime.expression.{Abstraction, Application, Expression, Variable}

object DeBruijnShifter:
    private def traverse(expression: Expression, place: Int, cutoff: Int): Expression =
        expression match
            case variable @ Variable(_) =>
                variable match
                    case deBruijn: DeBruijnVariable =>
                        if deBruijn.index >= cutoff then deBruijn.index += place
                        deBruijn
                    case _ => variable

            case abstraction @ Abstraction(parameter, body) =>
                val b = traverse(body, place, cutoff + 1)
                abstraction.copy(body = b)

            case application @ Application(callable, argument) =>
                val c = traverse(callable, place, cutoff)
                val a = traverse(argument, place, cutoff)
                application.copy(callable = c, argument = a)

    def shift[E <: Expression](expression: E, place: Int): E =
        traverse(expression, place, 1).asInstanceOf[E]
