package fprime.simple

import fprime.expression.{*, given}
import fprime.parser.Parser
import fprime.parser.combinators.map
import fprime.parsing.{Parsable, Tokens, summonParser}
import fprime.simple.SimpleType.SimpleTypeParser

private type SimplyTypedVariableInner = Variable[SimplyTypedTerm]
private type SimplyTypedAbstractionInner =
    Abstraction[Ascription[Variable[SimplyTypedTerm], SimpleType], SimplyTypedTerm]
private type SimplyTypedApplicationInner = Application[SimplyTypedTerm, SimplyTypedTerm]

type SimplyTypedTerm = SimplyTypedVariable | SimplyTypedAbstraction | SimplyTypedApplication

class SimplyTypedVariable(symbol: Symbol) extends SimplyTypedVariableInner(symbol)

object SimplyTypedVariable:
    def unapply(variable: SimplyTypedVariable): SimplyTypedVariableInner = variable

class SimplyTypedAbstraction(
    parameter: Ascription[Variable[SimplyTypedTerm], SimpleType],
    body: SimplyTypedTerm,
) extends SimplyTypedAbstractionInner(parameter, body)

object SimplyTypedAbstraction:
    def unapply(abstraction: SimplyTypedAbstraction): SimplyTypedAbstractionInner = abstraction

class SimplyTypedApplication(callable: SimplyTypedTerm, argument: SimplyTypedTerm)
    extends SimplyTypedApplicationInner(callable, argument)

object SimplyTypedApplication:
    def unapply(application: SimplyTypedApplication): SimplyTypedApplicationInner = application

object SimplyTypedTerm:
    private given Conversion[SimplyTypedVariableInner, SimplyTypedVariable] =
        variable => SimplyTypedVariable(variable.symbol)

    private given Conversion[SimplyTypedAbstractionInner, SimplyTypedAbstraction] =
        abstraction => SimplyTypedAbstraction(abstraction.parameter, abstraction.body)

    private given Conversion[SimplyTypedApplicationInner, SimplyTypedApplication] =
        application => SimplyTypedApplication(application.callable, application.argument)

    given SimplyTypedTermParser: Parsable[SimplyTypedTerm] with
        override lazy val parser: Parser[Tokens, SimplyTypedTerm] =
            // cast safety: the correct parser was supplied by the local given
            // match safety: a default parser that fails early is always defined
            summonParser[Expression].map {
                case e: SimplyTypedVariableInner @unchecked    => e
                case e: SimplyTypedAbstractionInner @unchecked => e
                case e: SimplyTypedApplicationInner @unchecked => e
                case _                                         => throw Exception("unreachable")
            }
