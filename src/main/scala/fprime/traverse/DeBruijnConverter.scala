package fprime.traverse

import fprime.expression.*

import scala.collection.mutable

object DeBruijnConverter:
    private class Context(
        var numFreeVariables: Int,
        val variableScopes: mutable.Map[Expression, mutable.Stack[Int]],
    )

    private def traverse[E <: Expression](expression: E, currentScope: Int)(using
        context: Context
    ): E =
        expression match
            case variable @ Variable(symbol, index) =>
                val scopes = context.variableScopes.getOrElseUpdate(variable, mutable.Stack())
                val bindingScope = scopes.topOption() match
                    case Some(scope) => scope
                    case None =>
                        context.numFreeVariables += 1
                        val scope = -context.numFreeVariables
                        scopes.push(scope)
                        scope
                variable.copy(index = currentScope - bindingScope).asInstanceOf[variable.type]

            case abstraction @ Abstraction(parameter, body) =>
                val scopes = context.variableScopes.getOrElseUpdate(parameter, mutable.Stack())
                scopes.push(currentScope)
                val b = traverse(body, currentScope + 1)
                scopes.pop()
                abstraction.copy(body = b).asInstanceOf[abstraction.type]

            case application @ Application(callable, argument) =>
                val c = traverse(callable, currentScope)
                val a = traverse(argument, currentScope)
                application.copy(callable = c, argument = a).asInstanceOf[application.type]

            case Ascription(_, _) => ???

    def convert[E <: Expression](expression: E): E =
        given Context(0, mutable.Map())
        traverse(expression, 0)

extension [T](stack: mutable.Stack[T])
    def topOption(): Option[T] = if stack.isEmpty then None else Some(stack.top)
