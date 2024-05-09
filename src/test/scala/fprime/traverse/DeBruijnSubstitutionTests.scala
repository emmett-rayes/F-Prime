package fprime.traverse

import fprime.untyped.UntypedTerm
import fprime.untyped.UntypedTerm.UntypedTermParser
import fprime.util.parse
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class DeBruijnSubstitutionTests extends AnyFunSuite:
    test("substitution variable") {
        val term = parse[UntypedTerm]("(b (λx.λy.b))")
        val converted = DeBruijnConverter.convert(term)
        val replacement = parse[UntypedTerm]("a")
        val substituted = DeBruijnSubstitution.substitute(converted, target = 1, replacement)
        val pretty = PrettyPrinter.pretty(substituted, mode = PrettyPrinter.Mode.Named)
        assert(pretty == "a (λx. λy. a)")
    }

    test("substitution term") {
        val term = parse[UntypedTerm]("b (λx.b)")
        val converted = DeBruijnConverter.convert(term)
        val replacement = parse[UntypedTerm]("a (λz.a)")
        val replacementDeBruijn = DeBruijnConverter.convert(replacement)
        val substituted =
            DeBruijnSubstitution.substitute(converted, target = 1, replacementDeBruijn)
        val pretty = PrettyPrinter.pretty(substituted, mode = PrettyPrinter.Mode.Named)
        assert(pretty == "a (λz. a) (λx. a (λz. a))")
    }
