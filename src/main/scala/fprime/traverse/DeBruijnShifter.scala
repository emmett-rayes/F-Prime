package fprime.traverse

import fprime.expression.*

object DeBruijnShifter:
    private def traverse[E <: Expression](expression: E, place: Int, cutoff: Int): E =
        expression match
            case constant @ Constant(_) => constant

            case variable @ Variable(_, index) =>
                if index < cutoff then variable
                else variable.copy(index = index + place).asInstanceOf[variable.type]

            case abstraction @ Abstraction(parameter, body) =>
                val b = traverse(body, place, cutoff + 1)
                abstraction.copy(body = b).asInstanceOf[abstraction.type]

            case application @ Application(callable, argument) =>
                val c = traverse(callable, place, cutoff)
                val a = traverse(argument, place, cutoff)
                application.copy(callable = c, argument = a).asInstanceOf[application.type]

            case ascription @ Ascription(expression, targetType) =>
                val e = traverse(expression, place, cutoff)
                val t = traverse(targetType, place, cutoff)
                ascription.copy(expression = e, `type` = t).asInstanceOf[ascription.type]

    def shift[E <: Expression](expression: E, place: Int): E =
        traverse(expression, place, 1)
