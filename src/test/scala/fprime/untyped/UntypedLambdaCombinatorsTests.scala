package fprime.untyped

import fprime.traverse.{DeBruijnConverter, PrettyPrinter}
import fprime.util.parseTerm
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class UntypedLambdaCombinatorsTests extends AnyFunSuite:
    private def process(input: String): String =
        val term = parseTerm(input)
        val converted = DeBruijnConverter.convert(term)
        PrettyPrinter.pretty(converted, mode = PrettyPrinter.Mode.NamelessLocals)

    test("identity") {
        assert(process("λx. x") == "λ 1")
    }

    test("constant") {
        assert(process("λx y. x") == "λ λ 2")
    }

    test("substitution") {
        assert(process("λx y z. x z (y z)") == "λ λ λ 3 1 (2 1)")
    }

    test("iota") {
        assert(process("λx. x S K") == "λ 1 S K")
    }

    test("composition") {
        assert(process("λx y z. x (y z)") == "λ λ λ 3 (2 1)")
    }

    test("swapping") {
        assert(process("λ x y z. x z y") == "λ λ λ 3 1 2")
    }

    test("duplicating") {
        assert(process("λx y. x y y") == "λ λ 2 1 1")
    }

    test("self application") {
        assert(process("λx. x x") == "λ 1 1")
    }

    test("divergent") {
        assert(process("(λx. x x) (λx. x x)") == "(λ 1 1) (λ 1 1)")
    }

    test("lazy fixed point") {
        assert(process("λf. (λx. f (x x)) (λx. f (x x))") == "λ (λ 2 (1 1)) (λ 2 (1 1))")
    }

    test("strict fixed point") {
        assert(
          process(
            "λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))"
          ) == "λ (λ 2 (λ 2 2 1)) (λ 2 (λ 2 2 1))"
        )
    }

    test("turing fixed point") {
        assert(
          process("(λx y. y (x x y)) (λx y. y (x x y))") == "(λ λ 1 (2 2 1)) (λ λ 1 (2 2 1))"
        )
    }

    test("reverse application") {
        assert(process("λx f. f x") == "λ λ 1 2")
    }
