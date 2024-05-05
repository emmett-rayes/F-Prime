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

case class UntypedVariable(override val symbol: Symbol) extends UntypedVariableInner(symbol)

case class UntypedAbstraction(
    override val parameter: UntypedVariable,
    override val body: UntypedLambda,
) extends UntypedAbstractionInner(parameter, body)

case class UntypedApplication(
    override val callable: UntypedLambda,
    override val argument: UntypedLambda,
) extends UntypedApplicationInner(callable, argument)

private given Conversion[UntypedVariableInner, UntypedVariable] =
    variable => UntypedVariable(variable.symbol)

private given Conversion[UntypedAbstractionInner, UntypedAbstraction] =
    abstraction => UntypedAbstraction(abstraction.parameter, abstraction.body)

private given Conversion[UntypedApplicationInner, UntypedApplication] =
    application => UntypedApplication(application.callable, application.argument)

given Parsable[UntypedLambda] with
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
