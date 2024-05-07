package fprime.traverse

import fprime.expression.{Abstraction, Application, Expression, Symbol, Variable}

import scala.collection.mutable

type DeBruijnIndex = Int

class DeBruijnVariable(var index: DeBruijnIndex, symbol: Symbol) extends Variable(symbol)

object DeBruijnConverter:
    private class Context(
        var numFreeVariables: Int,
        val variableScopes: mutable.Map[Expression, mutable.Stack[Int]],
    )

    private def traverse(expression: Expression, currentScope: Int)(using
        context: Context
    ): Expression =
        expression match
            case variable @ Variable(symbol) =>
                val scopes = context.variableScopes.getOrElseUpdate(variable, mutable.Stack())
                val bindingScope = scopes.topOption() match
                    case Some(scope) => scope
                    case None =>
                        context.numFreeVariables += 1
                        val scope = -context.numFreeVariables
                        scopes.push(scope)
                        scope
                DeBruijnVariable(currentScope - bindingScope, symbol)

            case abstraction @ Abstraction(parameter, body) =>
                val scopes = context.variableScopes.getOrElseUpdate(parameter, mutable.Stack())
                scopes.push(currentScope)
                val b = traverse(body, currentScope + 1)
                scopes.pop()
                abstraction.copy(body = b)

            case application @ Application(callable, argument) =>
                val c = traverse(callable, currentScope)
                val a = traverse(argument, currentScope)
                application.copy(callable = c, argument = a)

    def convert[E <: Expression](expression: E): E =
        given Context(0, mutable.Map())
        traverse(expression, 0).asInstanceOf[E]

extension [T](stack: mutable.Stack[T])
    def topOption(): Option[T] = if stack.isEmpty then None else Some(stack.top)
