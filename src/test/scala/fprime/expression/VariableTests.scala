package fprime.expression

import fprime.parsing.{asTokens, summonParser}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

import scala.util.{Failure, Success}

class VariableTests extends AnyFunSuite:
    test("variable parser (green)") {
        val input = "x".asTokens
        summonParser[Variable].parse(input) should matchPattern {
            case Success((_, Variable(symbol, _))) if symbol == "x" =>
        }
    }

    test("variable parser (red)") {
        val input = "->".asTokens
        summonParser[Variable].parse(input) should matchPattern { case Failure(_) => }
    }
