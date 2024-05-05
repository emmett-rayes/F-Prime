package fprime.expression

import fprime.parser.Parser

export Parsing.{*, given}

object Parsing:
    private type Token = Char
    type Tokens = IndexedSeq[Token]

    inline def summonParser[T](using t: Parsable[T]): Parser[Tokens, T] = t.parser

    trait Parsable[T]:
        lazy val parser: Parser[Tokens, T]

    object Tokens:
        def make(string: String): Tokens = string

    extension (tokens: Tokens) def skipWhitespace: Tokens = tokens.splitAfter(_.isWhitespace)._2

extension [A](seq: IndexedSeq[A])
    private def splitAfter(p: A => Boolean): (IndexedSeq[A], IndexedSeq[A]) =
        val index = seq.indexWhere(!p(_))
        seq.splitAt(if index == -1 then seq.size else index)

extension (c: Char)
    def isAsciiLetter: Boolean =
        c.isLetter && (('A' <= c && c <= 'z') || ('a' <= c && c <= 'z'))

    def isAsciiLetterOrDigit: Boolean = c.isDigit || c.isAsciiLetter
