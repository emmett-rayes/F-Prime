package fprime.simple

import fprime.expression.*
import fprime.traverse.PrettyPrinter

object SimpleTypeChecker:
    private type Context = Map[Variable[SimplyTypedTerm], SimpleType]

    private def traverse(expression: SimplyTypedTerm)(using
        context: Context
    ): Option[SimpleType] =
        expression match
            case variable @ SimplyTypedVariable(_, _) => context.get(variable)

            case SimplyTypedAbstraction(parameter, body) =>
                given Context = context.updated(parameter.expression, parameter.`type`)
                traverse(body).map(SimpleFunctionType(parameter.`type`, _))

            case SimplyTypedApplication(callable, argument) =>
                traverse(callable) match
                    case None => None
                    case Some(_type) =>
                        _type match
                            case SimpleUninterpretedType(_) => None
                            case SimpleFunctionType(Application(_, source), target) =>
                                if traverse(argument).contains(source) then Some(target)
                                else None

    def typeof(expression: SimplyTypedTerm): Option[SimpleType] =
        given Context = Map()
        traverse(expression)
