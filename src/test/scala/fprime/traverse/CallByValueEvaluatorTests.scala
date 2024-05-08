package fprime.traverse

import fprime.util.parseTerm
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class CallByValueEvaluatorTests extends AnyFunSuite:
    test("full beta") {
        val term = parseTerm("(λn.λs.λz.s (n s z)) (λs.λz.z)")
        val converted = DeBruijnConverter.convert(term)
        val reduced = CallByValueEvaluator.reduce(converted, normalize = true).last
        val pretty = PrettyPrinter.pretty(reduced, mode = PrettyPrinter.Mode.Named)
        assert(pretty == "λs. λz. s z")
    }
