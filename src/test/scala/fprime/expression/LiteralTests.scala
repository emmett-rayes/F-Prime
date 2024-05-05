package fprime.expression

import fprime.expression.Literal
import fprime.parsing.asTokens
import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

import scala.util.{Failure, Success}

class LiteralTests extends AnyFunSuite:
    test("literal parser (green)") {
        val parser = Literal.parser("hello")
        val input = "hello, world!".asTokens
        parser.parse(input) should matchPattern {
            case Success((remaining, output))
                if output == "hello" && remaining == ", world!".asTokens =>
        }
    }

    test("literal parser (red)") {
        val parser = Literal.parser("hello")
        val input = "goodbye, world!".asTokens
        parser.parse(input) should matchPattern { case Failure(_) => }
    }
