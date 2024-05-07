package fprime.parsing

import fprime.parser.Parser

type Token = Char
type Tokens = IndexedSeq[Token]

trait Parsable[T]:
    lazy val parser: Parser[Tokens, T]

inline def summonParser[T](using t: Parsable[T]): Parser[Tokens, T] = t.parser

extension (string: String) def asTokens: Tokens = string

extension (tokens: Tokens) def skipWhitespace: Tokens = tokens.splitAfter(_.isWhitespace)._2

extension [A](seq: IndexedSeq[A])
    def splitAfter(p: A => Boolean): (IndexedSeq[A], IndexedSeq[A]) =
        val index = seq.indexWhere(!p(_))
        seq.splitAt(if index == -1 then seq.size else index)
