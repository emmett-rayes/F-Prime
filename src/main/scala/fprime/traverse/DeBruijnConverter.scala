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
            case constant @ Constant(_) => constant

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
                val key = parameter match
                    case variable @ Variable(_, _)            => variable
                    case Ascription(variable: Variable[?], _) => variable
                    case _ =>
                        throw Exception(
                          "Currently, only a variable or an ascription is allowed as abstraction parameter"
                        )

                val scopes = context.variableScopes.getOrElseUpdate(key, mutable.Stack())
                scopes.push(currentScope)
                val b = traverse(body, currentScope + 1)
                scopes.pop()
                abstraction.copy(body = b).asInstanceOf[abstraction.type]

            case application @ Application(callable, argument) =>
                val c = traverse(callable, currentScope)
                val a = traverse(argument, currentScope)
                application.copy(callable = c, argument = a).asInstanceOf[application.type]

            case ascription @ Ascription(expression, targetType) =>
                val e = traverse(expression, currentScope)
                val t = traverse(targetType, currentScope)
                ascription.copy(expression = e, `type` = t).asInstanceOf[ascription.type]

    def convert[E <: Expression](expression: E): E =
        given Context(0, mutable.Map())
        traverse(expression, 0)

extension [T](stack: mutable.Stack[T])
    def topOption(): Option[T] = if stack.isEmpty then None else Some(stack.top)
