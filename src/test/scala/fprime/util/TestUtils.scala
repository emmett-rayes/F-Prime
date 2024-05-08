package fprime.util

import fprime.parsing.{asTokens, summonParser}
import fprime.untyped.{UntypedTerm, UntypedTermParser}

def parseTerm(string: String): UntypedTerm =
    val input = string.asTokens
    val (remaining, output) = summonParser[UntypedTerm].parse(input).get
    assert(remaining.isEmpty, s"Input was not fully parsed. Remaining: $remaining")
    output
