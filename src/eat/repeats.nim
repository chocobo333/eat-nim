
import strutils
import sugar

import parsers
import spanned
import utils


proc many0*[O](parser: Parser[O]): Parser[seq[O]] =
    newParser(
        proc(src: Spanned): PResult[seq[O]] =
            var
                res: seq[O] = @[]
                src = src
            while true:
                let tmp = parser.parse(src)
                if tmp.isErr:
                    return ok (src, res)
                if not parser.dropped:
                    res.add tmp.get
                src = tmp.getSrc
            return ok (src, res),
        genGraph("Many0", parser)
    )

proc many1*[O](parser: Parser[O]): Parser[seq[O]] =
    newParser(
        proc(src: Spanned): PResult[seq[O]] =
            var
                res: seq[O] = @[]
                src = src
            let tmp = parser.parse(src)
            if tmp.isErr:
                return err(tmp.src, tmp.getErr)
            res.add tmp.get
            src = tmp.getSrc
            while true:
                let tmp = parser.parse(src)
                if tmp.isErr:
                    return ok (src, res)
                if not parser.dropped:
                    res.add tmp.get
                src = tmp.getSrc
            return ok (src, res),
        genGraph("Many1", parser)
    )

proc manyMN*[O](parser: Parser[O], min, max: int): Parser[O] {.error("notimplemented").} =
    discard

proc times*[O](parser: Parser[O], n: int): Parser[seq[O]] =
    newParser(
        proc(src: Spanned): PResult[seq[O]] =
            var
                res: seq[O] = @[]
                src = src
            for i in 0..<n:
                let tmp = parser.parse(src)
                if tmp.isErr:
                    return err(tmp.src, tmp.getErr)
                if not parser.dropped:
                    res.add tmp.get
                src = tmp.getSrc
            return ok (src, res),
        genGraph("Times", parser, n)
    )

# TODO: make acc var
proc fold0*[O, T](parser: Parser[O], init: T, accumulator: (T, O) -> T): Parser[T] =
    newParser(
        proc(src: Spanned): PResult[T] =
            var
                res: T = init
                src = src
            while true:
                let tmp = parser.parse(src)
                if tmp.isErr:
                    return ok (src, res)
                if not parser.dropped:
                    res = res.accumulator(tmp.get)
                src = tmp.getSrc
            return ok (src, res),
        genGraph("Fold0", parser, ("$1: $2" % [$init, $T]), ("$1 -> $2" % [$(T, O), $T]))
    )