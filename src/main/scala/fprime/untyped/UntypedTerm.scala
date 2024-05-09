package fprime.untyped

import fprime.expression.*
import fprime.parser.Parser
import fprime.parser.combinators.map
import fprime.parsing.{Parsable, Tokens, summonParser}

private type UntypedVariableInner = Variable
private type UntypedAbstractionInner = Abstraction[UntypedVariable, UntypedTerm]
private type UntypedApplicationInner = Application[UntypedTerm, UntypedTerm]

type UntypedTerm = Expression & (UntypedVariable | UntypedAbstraction | UntypedApplication)

class UntypedVariable(symbol: Symbol) extends UntypedVariableInner(symbol)

object UntypedVariable:
    def unapply(variable: UntypedVariable): UntypedVariableInner = variable

class UntypedAbstraction(parameter: UntypedVariable, body: UntypedTerm)
    extends UntypedAbstractionInner(parameter, body)

object UntypedAbstraction:
    def unapply(abstraction: UntypedAbstraction): UntypedAbstractionInner = abstraction

class UntypedApplication(callable: UntypedTerm, argument: UntypedTerm)
    extends UntypedApplicationInner(callable, argument)

object UntypedApplication:
    def unapply(application: UntypedApplication): UntypedApplicationInner = application

object UntypedTerm:
    private given Conversion[UntypedVariableInner, UntypedVariable] =
        variable => UntypedVariable(variable.symbol)

    private given Conversion[UntypedAbstractionInner, UntypedAbstraction] =
        abstraction => UntypedAbstraction(abstraction.parameter, abstraction.body)

    private given Conversion[UntypedApplicationInner, UntypedApplication] =
        application => UntypedApplication(application.callable, application.argument)

    given UntypedTermParser: Parsable[UntypedTerm] with
        override lazy val parser: Parser[Tokens, UntypedTerm] =
            // cast safety: the correct parser was supplied by the local given
            summonParser[Expression].map {
                case e: UntypedVariableInner               => e
                case e: UntypedAbstractionInner @unchecked => e
                case e: UntypedApplicationInner @unchecked => e
            }
