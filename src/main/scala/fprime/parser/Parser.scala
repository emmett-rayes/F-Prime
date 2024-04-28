package fprime.parser

import scala.util.Try
import fprime.conversions.given

type ParserResult[Input, Output] = Try[(Input, Output)]

object Parser:
    def unit[I, O](output: O): Parser[I] { type Output = O } =
        new Parser[I] {
            override type Output = O
            override def parse(input: I) = (input, output)
        }

trait Parser[Input]:
    self =>
    type Output
    def parse(input: Input): ParserResult[Input, Output]

    final def flatMap[T](
        f: self.Output => Parser[Input] { type Output = T }
    ): Parser[Input] { type Output = T } = new Parser[Input] {
        override type Output = T
        override def parse(input: Input) =
            self.parse(input).flatMap((remaining, output) => f(output).parse(remaining))
    }

    final def orElse(
        other: Parser[Input]
    ): Parser[Input] { type Output = self.Output | other.Output } = new Parser[Input] {
        override type Output = self.Output | other.Output
        override def parse(input: Input) = self.parse(input).orElse(other.parse(input))
    }
