package fprime.expression

import fprime.parser.{ParseError, Parser}
import fprime.parsing.{Parsable, Tokens, asTokens, skipWhitespace}

import scala.util.Try

type Literal = Singleton & String

object Literal:
    given LiteralParser[S <: Literal](using expected: ValueOf[S]): Parsable[S] with
        override lazy val parser: Parser[Tokens, S] = (input: Tokens) =>
            val trimmed = input.skipWhitespace
            Try {
                if !trimmed.startsWith(expected.value.asTokens) then
                    throw ParseError(
                      input,
                      s"Expected ${expected.value} at this position. ${trimmed.head}",
                    )
                val (matched, remaining) = trimmed.splitAt(expected.value.length)
                (remaining, matched.mkString.asInstanceOf[S])
            }
