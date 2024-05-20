package fprime.expression

import fprime.expression.Literal.LiteralParser
import fprime.parsing.{asTokens, summonParser}
import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

import scala.util.Success

class LiteralTests extends AnyFunSuite:
    test("literal parser (green)") {
        val parser = summonParser["hello"]
        val input = "hello, world!".asTokens

        val result = parser.parse(input)
        assert(result.isSuccess)
        result match {
            case Success((remaining, output)) =>
                assert(output == "hello" && remaining == ", world!".asTokens)
        }
    }

    test("literal parser (red)") {
        val parser = summonParser["hello"]
        val input = "goodbye, world!".asTokens
        val result = parser.parse(input)
        assert(result.isFailure)
    }
