package fprime.repl

import fprime.parsing.{skipWhitespace, summonParser}
import fprime.simple.SimplyTypedTerm
import fprime.simple.SimplyTypedTerm.SimplyTypedTermParser
import fprime.traverse.{CallByValueEvaluator, DeBruijnConverter, PrettyPrinter}

import scala.io.StdIn.readLine
import scala.util.control.Breaks.{break, breakable}
import scala.util.{Failure, Success}

private def prompt(): String =
    print(">> ")
    readLine()

private def error(): Unit =
    println("!!")
    break

@main
def repl(): Nothing =
    val parser = summonParser[SimplyTypedTerm]
    while true do
        breakable {
            val input = prompt()
            val trace = for
                (remaining, term) <- parser.parse(input)
                _ = if remaining.skipWhitespace.nonEmpty then error()
                pretty = PrettyPrinter.pretty(term)
                converted = DeBruijnConverter.convert(term)
            yield CallByValueEvaluator.reduce(converted, normalize = true)
            trace match
                case Failure(exception) => error()
                case Success(trace) =>
                    if trace.isEmpty then println("stuck!")
                    if trace.size == 1 then println(PrettyPrinter.pretty(trace.head))
                    else
                        trace.zipWithIndex.foreach((term, i) =>
                            val pretty = PrettyPrinter.pretty(term)
                            println(s"$i. $pretty")
                        )
        }

    throw Exception("unreachable!")
