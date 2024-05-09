package fprime.simple

import fprime.expression.*
import fprime.parser.Parser
import fprime.parser.combinators.map
import fprime.parsing.{Parsable, Tokens, summonParser}

private type SimpleType = Variable

private type SimplyTypedVariableInner = Ascription[Variable, SimpleType]
private type SimplyTypedAbstractionInner = Abstraction[SimplyTypedVariable, SimplyTypedTerm]
private type SimplyTypedApplicationInner = Application[SimplyTypedTerm, SimplyTypedTerm]

type SimplyTypedTerm = Expression &
    (SimplyTypedVariable | SimplyTypedAbstraction | SimplyTypedApplication)

class SimplyTypedVariable(variable: Variable, `type`: SimpleType)
    extends SimplyTypedVariableInner(variable, `type`)

object SimplyTypedVariable:
    def unapply(variable: SimplyTypedVariable): SimplyTypedVariableInner = variable

class SimplyTypedAbstraction(parameter: SimplyTypedVariable, body: SimplyTypedTerm)
    extends SimplyTypedAbstractionInner(parameter, body)

object SimplyTypedAbstraction:
    def unapply(abstraction: SimplyTypedAbstraction): SimplyTypedAbstractionInner = abstraction

class SimplyTypedApplication(callable: SimplyTypedTerm, argument: SimplyTypedTerm)
    extends SimplyTypedApplicationInner(callable, argument)

object SimplyTypedApplication:
    def unapply(application: SimplyTypedApplication): SimplyTypedApplicationInner = application

object SimplyTypedTerm:
    private given Conversion[SimplyTypedVariableInner, SimplyTypedVariable] =
        ascription => SimplyTypedVariable(ascription.expression, ascription.`type`)

    private given Conversion[SimplyTypedAbstractionInner, SimplyTypedAbstraction] =
        abstraction => SimplyTypedAbstraction(abstraction.parameter, abstraction.body)

    private given Conversion[SimplyTypedApplicationInner, SimplyTypedApplication] =
        application => SimplyTypedApplication(application.callable, application.argument)

    given SimplyTypedTermParser: Parsable[SimplyTypedTerm] with
        override lazy val parser: Parser[Tokens, SimplyTypedTerm] =
            // cast safety: the correct parser was supplied by the local given
            summonParser[Expression].map {
                case e: SimplyTypedVariableInner @unchecked    => e
                case e: SimplyTypedAbstractionInner @unchecked => e
                case e: SimplyTypedApplicationInner @unchecked => e
                case _ => throw RuntimeException("unreachable")
                // match safety: a default parser that fails early is always defined
            }
