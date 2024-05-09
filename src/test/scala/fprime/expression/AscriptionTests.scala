package fprime.expression

import fprime.parsing.{asTokens, summonParser}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

import scala.util.Success

class AscriptionTests extends AnyFunSuite:
    test("ascription parser") {
        val input = "x : T".asTokens
        summonParser[Ascription[Variable, Variable]].parse(input) should matchPattern {
            case Success((_, Ascription(Variable(expression, _), Variable(targetType, _))))
                if expression == "x" && targetType == "T" =>
        }
    }
