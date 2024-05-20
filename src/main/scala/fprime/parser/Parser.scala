package fprime.parser

import scala.util.{Failure, Success, Try}

case class ParseError[Input](input: Input, message: String = null, cause: Throwable = null)
    extends Exception(message, cause)

type ParserResult[Input, Output] = Try[(Input, Output)]

extension [Input, Output](self: ParserResult[Input, Output])
    def remaining: Try[Input] = self.map(_._1)
    def result: Try[Output] = self.map(_._2)

trait Parser[Input, +Output]:
    def parse(input: Input): ParserResult[Input, Output]

    final def flatMap[Mapped](f: Output => Parser[Input, Mapped]): Parser[Input, Mapped] =
        input =>
            parse(input).flatMap((remaining, output) =>
                Try {
                    f(output).parse(remaining) match
                        case Failure(exception) => throw ParseError(input, cause = exception)
                        case Success(result)    => result
                }
            )

    final def orElse[Else](other: => Parser[Input, Else]): Parser[Input, Output | Else] =
        input => parse(input).orElse(other.parse(input))

object Parser:
    def unit[Input, Output](output: Output): Parser[Input, Output] = input => Try(input, output)
