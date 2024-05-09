package fprime.traverse

import fprime.expression.*

object DeBruijnSubstitution:
    private def traverse[E <: Expression](expression: E, target: Int, replacement: E): E =
        expression match
            case variable @ Variable(_, index) =>
                if index < 0 || index != target then variable
                else replacement.asInstanceOf[replacement.type]

            case abstraction @ Abstraction(parameter, body) =>
                DeBruijnShifter.shift(replacement, 1)
                val b = traverse(abstraction.body, target + 1, replacement)
                abstraction.copy(body = b).asInstanceOf[abstraction.type]

            case application @ Application(callable, argument) =>
                val c = traverse(application.callable, target, replacement)
                val a = traverse(application.argument, target, replacement)
                application.copy(callable = c, argument = a).asInstanceOf[application.type]

            case Ascription(_, _) => ???

    def substitute[E <: Expression](expression: E, target: Int, replacement: E): E =
        traverse(expression, target, replacement)
