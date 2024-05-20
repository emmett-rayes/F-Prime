package fprime.simple

import fprime.expression.*
import fprime.simple.SimplyTypedTerm.SimplyTypedTermParser
import fprime.util.parse
import org.scalatest.Inside.inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class SimplyTypedTermTests extends AnyFunSuite:
    test("simply typed variable") {
        val term = parse[SimplyTypedTerm]("λx: => A A.x")
        inside(term) { case SimplyTypedAbstraction(parameters, body) =>
            inside(parameters) { case Ascription(Variable(symbol, _), _type) =>
                assert(symbol == "x")
                inside(_type) {
                    case SimpleFunctionType(Application(_, source), target) =>
                        inside(source) {
                            case SimpleUninterpretedType(constant) => constant == "A"
                        }
                        inside(target) {
                            case SimpleUninterpretedType(constant) => constant == "A"
                        }
                }
            }
            inside(body) {
                case SimplyTypedVariable(symbol, _) => assert(symbol == "x")
            }
        }
    }
