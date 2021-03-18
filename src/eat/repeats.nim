
import strutils
import sugar

import parsers
import spanned
import utils


proc many0*[O](parser: Parser[O]): Parser[seq[O]] =
    result = proc(src: Spanned): PResult[seq[O]] =
        if src.enstring:
            return ok(genGraph("Many0", parser), @[])
        var
            res: seq[O] = @[]
            src = src
        while true:
            let tmp = parser(src)
            if tmp.isErr:
                return ok (src, res)
            src = tmp.getSrc
            res.add tmp.get
        return ok (src, res)

proc many1*[O](parser: Parser[O]): Parser[seq[O]] =
    result = proc(src: Spanned): PResult[seq[O]] =
        if src.enstring:
            return ok(genGraph("Many1", parser), @[])
        var
            res: seq[O] = @[]
            src = src
        let tmp = parser(src)
        if tmp.isErr:
            return err(tmp.src, tmp.getErr)
        res.add tmp.get
        src = tmp.getSrc
        while true:
            let tmp = parser(src)
            if tmp.isErr:
                return ok (src, res)
            src = tmp.getSrc
            res.add tmp.get
        return ok (src, res)

proc manyMN*[O](parser: Parser[O], min, max: int): Parser[O] {.error("notimplemented").} =
    discard

proc times*[O](parser: Parser[O], n: int): Parser[seq[O]] =
    result = proc(src: Spanned): PResult[seq[O]] =
        if src.enstring:
            return ok(genGraph("Times", parser, n), @[])
        var
            res: seq[O] = @[]
            src = src
        for i in 0..<n:
            let tmp = parser(src)
            if tmp.isErr:
                return err(tmp.src, tmp.getErr)
            src = tmp.getSrc
            res.add tmp.get
        return ok (src, res)

# TODO: make acc var
proc fold0*[O, T](parser: Parser[O], init: T, accumulator: (T, O) -> T): Parser[T] =
    result = proc(src: Spanned): PResult[T] =
        if src.enstring:
            return ok(genGraph("Fold0", parser, ("$1: $2" % [$init, $T]), ("$1 -> $2" % [$(T, O), $T])), T.default)
        var
            res: T = init
            src = src
        while true:
            let tmp = parser(src)
            if tmp.isErr:
                return ok (src, res)
            src = tmp.getSrc
            res = res.accumulator(tmp.get)
        return ok (src, res)

proc many0*[T, O](parser: IParser[T, O]): IParser[T, seq[O]] =
    result = proc(info: T, src: Spanned): PResult[seq[O]] =
        if src.enstring:
            return ok(genGraph("Many0", parser), @[])
        var
            res: seq[O] = @[]
            src = src
        while true:
            let tmp = info.parser(src)
            if tmp.isErr:
                return ok (src, res)
            src = tmp.getSrc
            res.add tmp.get
        return ok (src, res)

proc many1*[T, O](parser: IParser[T, O]): IParser[T, seq[O]] =
    result = proc(info: T, src: Spanned): PResult[seq[O]] =
        if src.enstring:
            return ok(genGraph("Many1", parser), @[])
        var
            res: seq[O] = @[]
            src = src
        let tmp = info.parser(src)
        if tmp.isErr:
            return err(tmp.src, tmp.getErr)
        res.add tmp.get
        src = tmp.getSrc
        while true:
            let tmp = info.parser(src)
            if tmp.isErr:
                return ok (src, res)
            src = tmp.getSrc
            res.add tmp.get
        return ok (src, res)

proc manyMN*[T, O](parser: IParser[T, O], min, max: int): IParser[T, O] {.error("notimplemented").} =
    discard

proc times*[T, O](parser: IParser[T, O], n: int): IParser[T, seq[O]] =
    result = proc(info: T, src: Spanned): PResult[seq[O]] =
        if src.enstring:
            return ok(genGraph("Times", parser, n), @[])
        var
            res: seq[O] = @[]
            src = src
        for i in 0..<n:
            let tmp = info.parser(src)
            if tmp.isErr:
                return err(tmp.src, tmp.getErr)
            src = tmp.getSrc
            res.add tmp.get
        return ok (src, res)

# TODO: make acc var
proc fold0*[T, O, A](parser: IParser[T, O], init: A, accumulator: (A, O) -> A): IParser[T, A] =
    result = proc(info: T, src: Spanned): PResult[T] =
        if src.enstring:
            return ok(genGraph("Fold0", parser, ("$1: $2" % [$init, $T]), ("$1 -> $2" % [$(T, O), $T])), T.default)
        var
            res: T = init
            src = src
        while true:
            let tmp = info.parser(src)
            if tmp.isErr:
                return ok (src, res)
            src = tmp.getSrc
            res = res.accumulator(tmp.get)
        return ok (src, res)