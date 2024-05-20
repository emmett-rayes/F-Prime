package fprime.expression

import fprime.expression.Ascription.AscriptionParser
import fprime.parsing.{asTokens, summonParser}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

import scala.util.Success

class AscriptionTests extends AnyFunSuite:
    test("ascription parser") {
        val input = "x : T".asTokens
        val parser = summonParser[Ascription[Variable[Expression], Constant["T"]]]
        val result = parser.parse(input)
        assert(result.isSuccess)
        result match
            case Success((_, Ascription(Variable(expression, _), Constant(targetType)))) =>
                assert(expression == "x" && targetType == "T")
            case _ =>
    }
