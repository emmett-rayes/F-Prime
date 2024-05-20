package fprime.simple

import fprime.expression.{*, given}
import fprime.parser.Parser
import fprime.parser.combinators.map
import fprime.parsing.{Parsable, Tokens, summonParser}

private type SimpleUninterpretedTypeInner = Constant["A"]

private type SimpleFunctionTypeInner =
    Application[Application[Constant["=>"], SimpleType], SimpleType]

type SimpleType = SimpleUninterpretedTypeInner | SimpleFunctionType

class SimpleUninterpretedType(constant: "A") extends SimpleUninterpretedTypeInner(constant)

object SimpleUninterpretedType:
    def unapply(uninterpreted: SimpleUninterpretedType): SimpleUninterpretedType = uninterpreted

class SimpleFunctionType(source: SimpleType, target: SimpleType)
    extends SimpleFunctionTypeInner(Application(Constant("=>"), source), target)

object SimpleFunctionType:
    def unapply(functionType: SimpleFunctionType): SimpleFunctionTypeInner = functionType

object SimpleType:
    private given Conversion[SimpleUninterpretedTypeInner, SimpleUninterpretedType] =
        predefined => SimpleUninterpretedType(predefined.constant)

    private given Conversion[SimpleFunctionTypeInner, SimpleFunctionType] =
        application => SimpleFunctionType(application.callable.argument, application.argument)

    given SimpleTypeParser: Parsable[SimpleType] with
        override lazy val parser: Parser[Tokens, SimpleType] =
            // cast safety: the correct parser was supplied by the local given
            // match safety: a default parser that fails early is always defined
            summonParser[Expression].map {
                case e: SimpleUninterpretedTypeInner @unchecked => e
                case e: SimpleFunctionTypeInner @unchecked      => e
                case _ => throw Exception("unreachable")
            }
