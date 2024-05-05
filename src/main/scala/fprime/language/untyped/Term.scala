package fprime.language.untyped

import fprime.expression.{Abstraction, Application, Expression, Variable, given}
import fprime.expression.Parsing.{Parsable, Tokens, summonParser}
import fprime.language.{Language, given}
import fprime.parser.{Parser, given}
import fprime.parser.combinators.map

type UntypedVariableInner = Variable
type UntypedAbstractionInner = Abstraction[UntypedVariable, UntypedLambda]
type UntypedApplicationInner = Application[UntypedLambda, UntypedLambda]

sealed trait UntypedLambda extends Language

case class UntypedVariable(term: UntypedVariableInner)
    extends UntypedLambda
    with Product1[UntypedVariableInner]

case class UntypedAbstraction(term: UntypedAbstractionInner)
    extends UntypedLambda
    with Product1[UntypedAbstractionInner]

case class UntypedApplication(term: UntypedApplicationInner)
    extends UntypedLambda
    with Product1[UntypedApplicationInner]

given Conversion[UntypedVariable, UntypedVariableInner] = _._1
given Conversion[UntypedVariableInner, UntypedVariable] = UntypedVariable(_)

given Conversion[UntypedAbstraction, UntypedAbstractionInner] = _._1
given Conversion[UntypedAbstractionInner, UntypedAbstraction] = UntypedAbstraction(_)

given Conversion[UntypedApplication, UntypedApplicationInner] = _._1
given Conversion[UntypedApplicationInner, UntypedApplication] = UntypedApplication(_)

given Parsable[UntypedLambda] with
    given Parsable[Abstraction[?, ?]] with
        override lazy val parser: Parser[Tokens, Abstraction[?, ?]] =
            summonParser[UntypedAbstraction]

    given Parsable[Application[?, ?]] with
        override lazy val parser: Parser[Tokens, Application[?, ?]] =
            summonParser[UntypedApplication]

    private val expression = summonParser[Expression]

    override lazy val parser: Parser[Tokens, UntypedLambda] =
        // cast safety: the correct parser was supplied by the local given
        summonParser[Expression].map {
            case e: UntypedVariableInner               => e
            case e: UntypedAbstractionInner @unchecked => e
            case e: UntypedApplicationInner @unchecked => e
        }
