package fprime.traverse

import fprime.util.parseTerm
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class DeBruijnSubstitutionTests extends AnyFunSuite:
    test("substitution variable") {
        val term = parseTerm("(b (λx.λy.b))")
        val converted = DeBruijnConverter.convert(term)
        val replacement = parseTerm("a")
        val substituted = DeBruijnSubstitution.substitute(converted, target = 1, replacement)
        val pretty = PrettyPrinter.pretty(substituted, mode = PrettyPrinter.Mode.Named)
        assert(pretty == "a (λx. λy. a)")
    }

    test("substitution term") {
        val term = parseTerm("b (λx.b)")
        val converted = DeBruijnConverter.convert(term)
        val replacement = parseTerm("a (λz.a)")
        val replacementDeBruijn = DeBruijnConverter.convert(replacement)
        val substituted =
            DeBruijnSubstitution.substitute(converted, target = 1, replacementDeBruijn)
        val pretty = PrettyPrinter.pretty(substituted, mode = PrettyPrinter.Mode.NamelessLocals)
        assert(pretty == "a (λ a) (λ a (λ a))")
    }
