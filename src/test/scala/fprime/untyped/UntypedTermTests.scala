package fprime.untyped

import fprime.untyped.UntypedTerm.UntypedTermParser
import fprime.util.parse
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class UntypedTermTests extends AnyFunSuite:
    test("variable") {
        val term = parse[UntypedTerm]("x")
        term should matchPattern { case UntypedVariable(symbol, _) if symbol == "x" => }
    }

    test("abstraction") {
        val term = parse[UntypedTerm]("λx. y x")
        term should matchPattern { case _: UntypedAbstraction => }
        val abstraction = term.asInstanceOf[UntypedAbstraction]
        assert(abstraction.parameter.symbol == "x")
        abstraction.body should matchPattern { case _: UntypedApplication => }
    }

    test("nested abstraction") {
        val term = parse[UntypedTerm]("λx y.x y z")
        term should matchPattern { case _: UntypedAbstraction => }
        val abstraction = term.asInstanceOf[UntypedAbstraction]
        abstraction.body should matchPattern { case _: UntypedAbstraction => }
    }

    test("application") {
        val term = parse[UntypedTerm]("(λx. x) (λx. x)")
        term should matchPattern { case UntypedApplication(_, _) => }
        val application = term.asInstanceOf[UntypedApplication]
        application.callable should matchPattern { case _: UntypedAbstraction => }
        application.argument should matchPattern { case _: UntypedAbstraction => }
    }

    test("application associativity") {
        val term = parse[UntypedTerm]("x y z")
        term should matchPattern { case UntypedApplication(_, _) => }
        val application = term.asInstanceOf[UntypedApplication]
        application.callable should matchPattern { case _: UntypedApplication => }
        application.argument should matchPattern { case _: UntypedVariable => }
    }

    test("complex term") {
        val term = parse[UntypedTerm]("λx. a (λt. b x t (f (λu. a u t z) λs. w)) w y")
        term should matchPattern { case _: UntypedAbstraction => }
    }
