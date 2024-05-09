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
                val b = traverse(body, target + 1, replacement)
                abstraction.copy(body = b).asInstanceOf[abstraction.type]

            case application @ Application(callable, argument) =>
                val c = traverse(callable, target, replacement)
                val a = traverse(argument, target, replacement)
                application.copy(callable = c, argument = a).asInstanceOf[application.type]

            case ascription @ Ascription(expression, targetType) =>
                val e = traverse(expression, target, replacement)
                val t = traverse(targetType, target, replacement)
                ascription.copy(expression = e, `type` = t).asInstanceOf[ascription.type]

    def substitute[E <: Expression](expression: E, target: Int, replacement: E): E =
        traverse(expression, target, replacement)
