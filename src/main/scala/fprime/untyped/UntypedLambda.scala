package fprime.untyped

import fprime.expression.{*, given}
import fprime.parser.Parser
import fprime.parser.combinators.map
import fprime.parsing.{Parsable, Tokens, summonParser}

private given [A <: B, B]: Conversion[A, B] = identity
private type UntypedVariableInner = Variable
private type UntypedAbstractionInner = Abstraction[UntypedVariable, UntypedLambda]
private type UntypedApplicationInner = Application[UntypedLambda, UntypedLambda]

type UntypedLambda = Expression & (UntypedVariable | UntypedAbstraction | UntypedApplication)

class UntypedVariable(symbol: Symbol) extends UntypedVariableInner(symbol)

object UntypedVariable:
    def unapply(variable: UntypedVariable): UntypedVariableInner = variable

class UntypedAbstraction(parameter: UntypedVariable, body: UntypedLambda)
    extends UntypedAbstractionInner(parameter, body)

object UntypedAbstraction:
    def unapply(abstraction: UntypedAbstraction): UntypedAbstractionInner = abstraction

class UntypedApplication(callable: UntypedLambda, argument: UntypedLambda)
    extends UntypedApplicationInner(callable, argument)

object UntypedApplication:
    def unapply(application: UntypedApplication): UntypedApplicationInner = application

private given Conversion[UntypedVariableInner, UntypedVariable] =
    variable => UntypedVariable(variable.symbol)

private given Conversion[UntypedAbstractionInner, UntypedAbstraction] =
    abstraction => UntypedAbstraction(abstraction.parameter, abstraction.body)

private given Conversion[UntypedApplicationInner, UntypedApplication] =
    application => UntypedApplication(application.callable, application.argument)

given UntypedLambdaParser: Parsable[UntypedLambda] with
    private given Parsable[Abstraction[?, ?]] with
        override lazy val parser: Parser[Tokens, Abstraction[?, ?]] =
            summonParser[UntypedAbstraction].map(_.asInstanceOf[Abstraction[?, ?]])

    private given Parsable[Application[?, ?]] with
        override lazy val parser: Parser[Tokens, Application[?, ?]] =
            summonParser[UntypedApplication].map(_.asInstanceOf[Application[?, ?]])

    override lazy val parser: Parser[Tokens, UntypedLambda] =
        // cast safety: the correct parser was supplied by the local given
        summonParser[Expression].map {
            case e: UntypedVariableInner               => e
            case e: UntypedAbstractionInner @unchecked => e
            case e: UntypedApplicationInner @unchecked => e
        }
