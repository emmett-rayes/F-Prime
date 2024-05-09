package fprime.simple

import fprime.simple.SimplyTypedTerm.SimplyTypedTermParser
import fprime.util.parse
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class SimplyTypedTermTests extends AnyFunSuite:
    test("simply typed variable") {
        val term = parse[SimplyTypedTerm]("x : Int")
        term should matchPattern {
            case SimplyTypedVariable(variable, t)
                if variable.symbol == "x" && t.symbol == "Int" =>
        }
    }
