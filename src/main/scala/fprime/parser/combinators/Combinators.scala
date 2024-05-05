package fprime.parser.combinators

import fprime.parser.{ParseError, Parser}

extension [Input, Output](self: => Parser[Input, Output])
    def map[Mapped](f: Output => Mapped): Parser[Input, Mapped] =
        self.flatMap(output => Parser.unit(f(output)))

    def repeated: Parser[Input, List[Output]] =
        val greedy = for
            selfOutput <- self
            otherOutputs <- repeated
        yield selfOutput :: otherOutputs
        greedy.orElse(Parser.unit(List()))

    def atLeast(n: Int): Parser[Input, List[Output]] =
        for outputs <- self.repeated
        yield
            if n <= outputs.size then outputs
            else throw ParseError(s"Expected at least ${n - outputs.size} more elements.")

    def andThen[Then](other: => Parser[Input, Then]): Parser[Input, (Output, Then)] =
        for
            selfOutput <- self
            otherOutput <- other
        yield (selfOutput, otherOutput)

    def skipThen[Then](other: => Parser[Input, Then]): Parser[Input, Then] =
        self.andThen(other).map(_._2)

    def thenSkip[Skip](other: => Parser[Input, Skip]): Parser[Input, Output] =
        self.andThen(other).map(_._1)

    def between[First, Second](
        first: => Parser[Input, First],
        second: => Parser[Input, Second],
    ): Parser[Input, Output] =
        first.skipThen(self).thenSkip(second)
