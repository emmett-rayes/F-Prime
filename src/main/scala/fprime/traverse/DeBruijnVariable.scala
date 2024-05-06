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
    private def convert[E <: Expression](expression: E, currentScope: Int)(using
        context: Context
    ): E =
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
                return DeBruijnVariable(currentScope - bindingScope, symbol).asInstanceOf[E]

            case abstraction @ Abstraction(parameter, body) =>
                val scopes = context.variableScopes.getOrElseUpdate(parameter, mutable.Stack())
                scopes.push(currentScope)
                abstraction.body = convert(body, currentScope + 1)
                scopes.pop()

            case application @ Application(callable, argument) =>
                application.callable = convert(callable, currentScope)
                application.argument = convert(argument, currentScope)

        expression

    def convert[E <: Expression](expression: E): E =
        given Context(0, mutable.Map())
        convert(expression, 0)

extension [T](stack: mutable.Stack[T])
    def topOption(): Option[T] = if stack.isEmpty then None else Some(stack.top)
