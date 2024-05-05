package fprime.expression

import fprime.expression.Parsing.{Tokens, skipWhitespace}
import fprime.parser.{ParseError, Parser}

import scala.util.Try

export Literal.{*, given}

object Literal:
    opaque type Literal = String

    def make(string: String): Literal = string

    def parser(expected: Literal): Parser[Tokens, Literal] = (input: Tokens) =>
        val trimmed = input.skipWhitespace
        Try {
            if !trimmed.startsWith(Tokens.make(expected)) then
                throw ParseError(s"Expected $expected at this position. ${trimmed.head}")
            val (matched, remaining) = trimmed.splitAt(expected.length)
            (remaining, matched.mkString)
        }

extension (t: Literal.type)
    def parser(string: String): Parser[Tokens, Literal] =
        Literal.parser(Literal.make(string))
