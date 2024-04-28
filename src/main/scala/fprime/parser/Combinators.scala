package fprime.parser

extension [Input](self: Parser[Input])
    def map[T](f: self.Output => T): Parser[Input] { type Output = T } =
        self.flatMap(output => Parser.unit(f(output)))

    def atLeast(n: Int): Parser[Input] { type Output = List[self.Output] } = n match
        case 0 => Parser.unit(List())
        case n =>
            for {
                selfOutput <- self
                otherOutputs <- atLeast(n - 1)
            } yield selfOutput :: otherOutputs

    def andThen(
        other: Parser[Input]
    ): Parser[Input] { type Output = (self.Output, other.Output) } =
        for
            selfOutput <- self
            otherOutput <- other
        yield (selfOutput, otherOutput)

    def skipThen(other: Parser[Input]): Parser[Input] { type Output = other.Output } =
        self.andThen(other).map(_._2)

    def thenSkip(other: Parser[Input]): Parser[Input] { type Output = self.Output } =
        self.andThen(other).map(_._1)

    def between(
        first: Parser[Input],
        second: Parser[Input]
    ): Parser[Input] { type Output = self.Output } =
        first.skipThen(self).thenSkip(second)
