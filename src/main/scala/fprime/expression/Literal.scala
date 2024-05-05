package fprime.expression

import fprime.parser.{ParseError, Parser}
import fprime.parsing.{Tokens, asTokens, skipWhitespace}

import scala.util.Try

type Literal = String

object Literal:
    def make(string: String): Literal = string

    def parser(expected: Literal): Parser[Tokens, Literal] = (input: Tokens) =>
        val trimmed = input.skipWhitespace
        Try {
            if !trimmed.startsWith(expected.asTokens) then
                throw ParseError(input, s"Expected $expected at this position. ${trimmed.head}")
            val (matched, remaining) = trimmed.splitAt(expected.length)
            (remaining, matched.mkString)
        }

extension (t: Literal.type)
    def parser(string: String): Parser[Tokens, Literal] =
        Literal.parser(Literal.make(string))
