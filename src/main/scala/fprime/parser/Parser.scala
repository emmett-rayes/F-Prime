package fprime.parser

import fprime.parser.combinators.map

import scala.util.Try

class ParseError(message: String) extends Exception(message)

type ParserResult[Input, Output] = Try[(Input, Output)]

extension [Input, Output](self: ParserResult[Input, Output])
    def remaining: Try[Input] = self.map(_._1)
    def result: Try[Output] = self.map(_._2)

given [I, A, B](using Conversion[A, B]): Conversion[Parser[I, A], Parser[I, B]] =
    _.map(identity)

trait Parser[Input, +Output]:
    def parse(input: Input): ParserResult[Input, Output]

    final def flatMap[Mapped](f: Output => Parser[Input, Mapped]): Parser[Input, Mapped] =
        input => parse(input).flatMap((remaining, output) => f(output).parse(remaining))

    final def orElse[Else](other: => Parser[Input, Else]): Parser[Input, Output | Else] =
        input => parse(input).orElse(other.parse(input))

object Parser:
    def unit[Input, Output](output: Output): Parser[Input, Output] = input => Try(input, output)
