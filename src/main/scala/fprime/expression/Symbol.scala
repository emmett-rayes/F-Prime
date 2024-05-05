package fprime.expression

import fprime.expression.Parsing.{Parsable, Tokens, skipWhitespace}
import fprime.parser.{ParseError, Parser}

import scala.util.Try

export Symbol.{*, given}

object Symbol:
    opaque type Symbol = String

    given Parsable[Symbol] with
        override lazy val parser: Parser[Tokens, Symbol] = (input: Tokens) =>
            val trimmed = input.skipWhitespace
            Try {
                if trimmed.isEmpty then throw ParseError("Expected input at this position.")
                if !trimmed.head.isAsciiLetter then
                    throw ParseError("Symbol must start with an ASCII letter.")
                val (matched, remaining) =
                    trimmed.splitAfter(t => t.isAsciiLetterOrDigit || t == '-' || t == '_')
                (remaining, matched.mkString)
            }
