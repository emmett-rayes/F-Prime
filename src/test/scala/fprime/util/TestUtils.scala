package fprime.util

import fprime.parsing.{asTokens, summonParser}
import fprime.untyped.{UntypedLambda, UntypedLambdaParser}

def parseTerm(string: String): UntypedLambda =
    val input = string.asTokens
    val (remaining, output) = summonParser[UntypedLambda].parse(input).get
    assert(remaining.isEmpty, s"Input was not fully parsed. Remaining: $remaining")
    output
