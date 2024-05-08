package fprime.repl

import fprime.parser.result
import fprime.parsing.summonParser
import fprime.traverse.{CallByValueEvaluator, DeBruijnConverter, PrettyPrinter}
import fprime.untyped.{UntypedTerm, UntypedTermParser}

import scala.io.StdIn.readLine
import scala.util.{Failure, Success}

private def prompt(): String =
    print(">> ")
    readLine()

private def error(): Unit = println("!!")

@main
def repl(): Nothing =
    while true do
        val input = prompt()

        val parser = summonParser[UntypedTerm]
        val trace = for
            term <- parser.parse(input).result
            pretty = PrettyPrinter.pretty(term)
            converted = DeBruijnConverter.convert(term)
        yield CallByValueEvaluator.reduce(converted, normalize = true)

        trace match
            case Failure(exception) => error()
            case Success(trace) =>
                if trace.isEmpty then println("stuck!")
                else
                    trace.zipWithIndex.foreach((term, i) =>
                        val pretty = PrettyPrinter.pretty(term)
                        println(s"$i. $pretty")
                    )

    throw RuntimeException("unreachable!")
