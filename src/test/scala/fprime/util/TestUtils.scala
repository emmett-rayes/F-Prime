package fprime.util

import fprime.parsing.{Parsable, asTokens}

def parse[T](string: String)(using parsable: Parsable[T]): T =
    val input = string.asTokens
    val (remaining, output) = parsable.parser.parse(input).get
    assert(remaining.isEmpty, s"Input was not fully parsed. Remaining: $remaining")
    output
