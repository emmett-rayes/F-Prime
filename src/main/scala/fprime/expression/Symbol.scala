package fprime.expression

import fprime.parser.{ParseError, Parser}
import fprime.parsing.{Parsable, Tokens, skipWhitespace, splitAfter}

import scala.util.Try

type Symbol = String

object Symbol:
    given SymbolParser: Parsable[Symbol] with
        override lazy val parser: Parser[Tokens, Symbol] = (input: Tokens) =>
            val trimmed = input.skipWhitespace
            Try {
                if trimmed.isEmpty then
                    throw ParseError(input, "Expected input at this position.")
                if !trimmed.head.isAsciiLetter then
                    throw ParseError(input, "Symbol must start with an ASCII letter.")
                val (matched, remaining) =
                    trimmed.splitAfter(t => t.isAsciiLetterOrDigit || t == '-' || t == '_')
                (remaining, matched.mkString)
            }

extension (c: Char)
    def isAsciiLetter: Boolean =
        c.isLetter && (('A' <= c && c <= 'z') || ('a' <= c && c <= 'z'))

    def isAsciiLetterOrDigit: Boolean = c.isDigit || c.isAsciiLetter
