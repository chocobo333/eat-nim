
import strutils
import sequtils
import options
import strformat
import sugar

import parsers
import nones
import repeats
import utils
import spanned

import macros


proc alt*[O](parsers: seq[Parser[O]]): Parser[O] =
    result = proc (src: Spanned): PResult[O] =
        if src.enstring:
            return ok(genGraphS("Alt", parsers), O.default)
        for parser in parsers:
            result = parser(src)
            if result.isOk:
                return

proc alt*[O](parsers: varargs[Parser[O]]): Parser[O] =
    alt(@parsers)

proc tupl*[O1, O2](a: Parser[O1], b: Parser[O2]): Parser[(O1, O2)] =
    type
        O = (O1, O2)
    result = proc(src: Spanned): PResult[O] =
        if src.enstring:
            return ok(genGraph("Tuple", a, b), (O1.default, O2.default))
        let resultA = a(src)
        if resultA.isOk:
            let
                resultB = b(resultA.getSrc)
                resultA = resultA.get
            if resultB.isOk:
                let
                    src = resultB.getSrc
                    resultB = resultB.get
                return ok(src, (resultA, resultB))
            else:
                return err(resultB.getSrc, resultB.getErr)
        else:
            return err(resultA.getSrc, resultA.getErr)

proc tupl*[O1, O2, O3](tup: (Parser[O1], Parser[O2], Parser[O3])): Parser[(O1, O2, O3)] =
    result = proc(src: Spanned): PResult[(O1, O2, O3)] =
        if src.enstring:
            return ok(genGraph("Tuple", tup[0], tup[1], tup[2]), (O1.default, O2.default, O3.default))
        var
            res: (O1, O2, O3)
            src = src
        let tmp0 = tup[0](src)
        if tmp0.isErr:
            return err(tmp0.getErr)
        res[0] = tmp0.get[1]
        src = tmp0.get[0]
        
        let tmp1 = tup[1](src)
        if tmp1.isErr:
            return err(tmp1.getErr)
        res[1] = tmp1.get[1]
        src = tmp1.get[0]

        let tmp2 = tup[2](src)
        if tmp2.isErr:
            return err(tmp2.getErr)
        res[2] = tmp0.get[1]
        src = tmp2.get[0]
        return ok (src, res)

proc sequence*[O](parsers: seq[Parser[O]]): Parser[seq[O]] =
    result = proc(src: Spanned): PResult[seq[O]] =
        if src.enstring:
            return ok(genGraphS("Sequence", parsers), @[])
        var
            res: seq[O] = @[]
            src = src
        for parser in parsers:
            let tmp = parser(src)
            if tmp.isErr:
                return err(tmp.getSrc, tmp.getErr)
            src = tmp.getSrc
            res.add tmp.get
        return ok (src, res)

proc sequence*[O](parsers: varargs[Parser[O]]): Parser[seq[O]] =
    sequence(@parsers)

proc map*[O1, O2](parser: Parser[O1], callback: O1 -> O2): Parser[O2] =
    result = proc(src: Spanned): PResult[O2] =
        if src.enstring:
            ok(genGraph("Map", parser, ("$1 -> $2" % [$O1, $O2])), O2.default)
        else:
            parser(src).map(callback)

proc mapRes*[O1, O2](parser: Parser[O1], callback: PResult[O1] -> PResult[O2]): Parser[O2] =
    result = proc(src: Spanned): PResult[O2] =
        if src.enstring:
            ok(genGraph("MapRes", parser, ("$1 -> $2" % [$PResult[O1], $PResult[O2]])), O2.default)
        else:
            parser(src).callback

proc mapErr*[O, E1, E2](parser: Parser[O], callback: E1 -> E2): Parser[O] =
    result = proc(src: Spanned): PResult[O] =
        if src.enstring:
            ok(genGraph("MapErr", parser, ("$1 -> $2" % [$E1, $E2])), O.default)
        else:
            parser(src).mapErr(callback)

proc opt*[O](parser: Parser[O]): Parser[Option[O]] =
    result = proc(src: Spanned): PResult[Option[O]] =
        if src.enstring:
            return ok(genGraph("Opt", parser), none O)
        let tmp = parser(src)
        if tmp.isOk:
            ok (tmp.getSrc, some tmp.get)
        else:
            ok (src, none(O))
    
proc flatten[T](self: seq[seq[T]]): seq[T] =
    result = @[]
    for e in self:
        result.add e

proc separated1*[O1, O2](parser: Parser[O1], delimiter: Parser[O2]): Parser[seq[O1]] =
    let tmp = tupl(delimiter, parser).map(it => it[1])
    tupl(parser, many0(tmp)).map(it => it[0] & it[1]).overwrite(
        genGraph("Separated1", parser, delimiter)
    )

proc separated0*[O1, O2](parser: Parser[O1], delimiter: Parser[O2]): Parser[seq[O1]] =
    alt(separated1(parser, delimiter), nones.none[seq[O1]]()).overwrite(
        genGraph("Separated0", parser, delimiter)
    )

proc terminated*[O1, O2](parser: Parser[O1], terminator: Parser[O2]): Parser[O1] =
    # newParser(
    #     proc(src: Spanned): PResult[O1] =
    #         result = parser(src)
    #         if result.isOk:
    #             let tmp = terminator(result.get[0])
    #             if tmp.isOk:
    #                 return ok (tmp.get[0], result.get[1])
    #             else:
    #                 return err tmp.getErr
    #         else:
    #             return
    # )
    tupl(parser, terminator).map(it=>it[0]).overwrite(
        genGraph("Terminated", parser, terminator)
    )
proc preceded*[O1, O2](precedor: Parser[O1], parser: Parser[O2]): Parser[O2] =
    # tupl(la(precedor), tupl(precedor, parser)).map(it=>it[1][1])
    tupl(precedor, parser).map(it=>it[1]).overwrite(
        genGraph("Preceded", precedor, parser)
    )

proc delimited*[O1, O2, O3](a: Parser[O1], b: Parser[O2], c: Parser[O3]): Parser[O2] =
    tupl(tupl(a, b), c).map(it=>it[0][1]).overwrite(
        genGraph("Delimited", a, b, c)
    )

proc surrounded*[O1, O2](outer: Parser[O1], inner: Parser[O2]): Parser[O2] =
    delimited(outer, inner, outer).overwrite(
        genGraph("Surronded", outer, inner)
    )

proc alt*[T, O](parsers: seq[IParser[T, O]]): IParser[T, O] =
    result = proc (info: ref T, src: Spanned): PResult[O] =
        if src.enstring:
            return ok(genGraphS("Alt", parsers), O.default)
        var tmp: ref T
        new(tmp)
        tmp[] = info[]
        for parser in parsers:
            result = info.parser(src)
            if result.isOk:
                return
            info[] = tmp[]

proc alt*[T, O](parsers: varargs[IParser[T, O]]): IParser[T, O] =
    alt(@parsers)

proc tupl*[T, O1, O2](a: IParser[T, O1], b: IParser[T, O2]): IParser[T, (O1, O2)] =
    type
        O = (O1, O2)
    result = proc(info: ref T, src: Spanned): PResult[O] =
        if src.enstring:
            return ok(genGraph("Tuple", a, b), O.default)
        var tmp: ref T
        new(tmp)
        tmp[] = info[]
        let resultA = info.a(src)
        if resultA.isOk:
            let
                resultB = info.b(resultA.getSrc)
                resultA = resultA.get
            if resultB.isOk:
                let
                    src = resultB.getSrc
                    resultB = resultB.get
                return ok(src, (resultA, resultB))
            else:
                info[] = tmp[]
                return err(resultB.getSrc, resultB.getErr)
        else:
            info[] = tmp[]
            return err(resultA.getSrc, resultA.getErr)

proc tupl*[T, O1, O2, O3](tup: (IParser[T, O1], IParser[T, O2], IParser[T, O3])): IParser[T, (O1, O2, O3)] =
    result = proc(info: ref T, src: Spanned): PResult[(O1, O2, O3)] =
        if src.enstring:
            return ok(genGraph("Tuple", tup[0], tup[1], tup[2]), (O1.default, O2.default, O3.default))
        var
            res: (O1, O2, O3)
            src = src
        let tmp0 = tup[0](info, src)
        if tmp0.isErr:
            return err(tmp0.getErr)
        res[0] = tmp0.get[1]
        src = tmp0.get[0]
        
        let tmp1 = tup[1](info, src)
        if tmp1.isErr:
            return err(tmp1.getErr)
        res[1] = tmp1.get[1]
        src = tmp1.get[0]

        let tmp2 = tup[2](info, src)
        if tmp2.isErr:
            return err(tmp2.getErr)
        res[2] = tmp0.get[1]
        src = tmp2.get[0]
        return ok (src, res)

proc sequence*[T, O](parsers: seq[IParser[T, O]]): IParser[T, seq[O]] =
    result = proc(info: ref T, src: Spanned): PResult[seq[O]] =
        if src.enstring:
            return ok(genGraphS("Sequence", parsers), @[])
        var
            res: seq[O] = @[]
            src = src
        for parser in parsers:
            let tmp = info.parser(src)
            if tmp.isErr:
                return err(tmp.getSrc, tmp.getErr)
            src = tmp.getSrc
            res.add tmp.get
        return ok (src, res)

proc sequence*[T, O](parsers: varargs[IParser[T, O]]): IParser[T, seq[O]] =
    sequence(@parsers)

proc map*[T, O1, O2](parser: IParser[T, O1], callback: O1 -> O2): IParser[T, O2] =
    result = proc(info: ref T, src: Spanned): PResult[O2] =
        if src.enstring:
            ok(genGraph("Map", parser, ("$1 -> $2" % [$O1, $O2])), O2.default)
        else:
            info.parser(src).map(callback)

proc mapRes*[T, O1, O2](parser: IParser[T, O1], callback: PResult[O1] -> PResult[O2]): IParser[T, O2] =
    result = proc(info: ref T, src: Spanned): PResult[O2] =
        if src.enstring:
            ok(genGraph("MapRes", parser, ("$1 -> $2" % [$PResult[O1], $PResult[O2]])), O2.default)
        else:
            info.parser(src).callback

proc mapErr*[T, O, E1, E2](parser: IParser[T, O], callback: E1 -> E2): IParser[T, O] =
    result = proc(info: ref T, src: Spanned): PResult[O] =
        if src.enstring:
            ok(genGraph("MapErr", parser, ("$1 -> $2" % [$E1, $E2])), O.default)
        else:
            info.parser(src).mapErr(callback)

proc opt*[T, O](parser: IParser[T, O]): IParser[T, Option[O]] =
    result = proc(info: ref T, src: Spanned): PResult[Option[O]] =
        if src.enstring:
            return ok(genGraph("Opt", parser), none O)
        # var tmpinfo: ref T
        # new(tmpinfo)
        # tmpinfo[] = info[]
        let tmp = info.parser(src)
        if tmp.isOk:
            ok (tmp.getSrc, some tmp.get)
        else:
            # info[] = tmpinfo[]
            ok (src, none(O))

proc separated1*[T, O1, O2](parser: IParser[T, O1], delimiter: IParser[T, O2]): IParser[T, seq[O1]] =
    let tmp = tupl(delimiter, parser).map(it => it[1])
    tupl(parser, many0(tmp)).map(it => it[0] & it[1]).overwrite(
        genGraph("Separated1", parser, delimiter)
    )

proc separated0*[T, O1, O2](parser: IParser[T, O1], delimiter: IParser[T, O2]): IParser[T, seq[O1]] =
    alt(separated1(parser, delimiter), nones.none[seq[O1]]().toIParser(T)).overwrite(
        genGraph("Separated0", parser, delimiter)
    )

proc terminated*[T, O1, O2](parser: IParser[T, O1], terminator: IParser[T, O2]): IParser[T, O1] =
    tupl(parser, terminator).map(it=>it[0]).overwrite(
        genGraph("Terminated", parser, terminator)
    )

proc preceded*[T, O1, O2](precedor: IParser[T, O1], parser: IParser[T, O2]): IParser[T, O2] =
    # tupl(la(precedor), tupl(precedor, parser)).map(it=>it[1][1])
    tupl(precedor, parser).map(it=>it[1]).overwrite(
        genGraph("Preceded", precedor, parser)
    )

proc delimited*[T, O1, O2, O3](a: IParser[T, O1], b: IParser[T, O2], c: IParser[T, O3]): IParser[T, O2] =
    tupl(tupl(a, b), c).map(it=>it[0][1]).overwrite(
        genGraph("Delimited", a, b, c)
    )

proc surrounded*[T, O1, O2](outer: IParser[T, O1], inner: IParser[T, O2]): IParser[T, O2] =
    delimited(outer, inner, outer).overwrite(
        genGraph("Surronded", outer, inner)
    )

when isMainModule:
    import patterns
    import strutils

    let
        a = pattern"a*"
        b = pattern"b*"
    echo alt(@[a, b])
    assert alt(@[a, b])("aaabbb").unwrap.fragment == "aaa"
    assert alt(a, b)("aaabbb").unwrap.fragment == "aaa"
    assert alt(a, b)("bbbaaabbb").unwrap.fragment == "bbb"
    echo tupl(b, a)
    assert tupl(b, a)("bbbaaabbb").unwrap[1].fragment == "aaa"
    echo sequence(@[a, b])
    assert sequence(@[a, b])("bbbaaabbb").isErr
    echo sequence(b, a, b)
    echo sequence(b, a, b)("bbbaaabbb")
    assert sequence(b, a, b)("bbbaaabbb").unwrap.mapIt(it.fragment).join() == "bbbaaabbb"
    assert many0(str"b")("bbbbb").unwrap.mapIt(it.fragment).join() == "bbbbb"
    assert many0(str"a")("bbbbb").unwrap.mapIt(it.fragment).join() == ""
    assert many1(str"a")("bbbbb").isErr
    echo many1(str"a")
    assert many1(str"b")("bbbbb").unwrap.mapIt(it.fragment).join() == "bbbbb"
    echo sequence(a, b).map(it => it.join)
    assert sequence(a.map(it => it.fragment), b.map(it => it.fragment)).map(it => it.join)("aabbaa").unwrap == "aabb"
    echo opt(str"+")
    assert opt(str"+")("3").unwrap.isNone
    assert opt(str"+")("+3").unwrap.get.fragment == "+"
    echo separated1(str"a", str(","))
    assert separated1(str"a", str(","))("a,a,a,a").unwrap.mapIt(it.fragment) == @["a", "a", "a", "a"]
    assert separated1(str"b", str(","))("a,a,a,a").isErr
    echo separated0(str"b", str(","))
    assert separated0(str"b", str(","))("a,a,a,a").unwrap.mapIt(it.fragment) == @[]
    assert separated0(str"a", str(","))("a,a,a,a").unwrap.mapIt(it.fragment) == @["a", "a", "a", "a"]
    echo terminated(pattern"[0-9]+", str";")
    assert terminated(pattern"[0-9]+", str";")("123;").unwrap.fragment == "123"
    assert terminated(pattern"[0-9]+", str";")("123:").isErr
    assert terminated(pattern"[0-9]+", str";")("w123:").isErr
    echo preceded(pattern"[0-9]+", str";")
    assert preceded(pattern"[0-9]+", str";")("123;").unwrap.fragment == ";"
    assert preceded(pattern"[0-9]+", str";")("abc;").isErr
    assert preceded(pattern"[0-9]+", str";")("123:").isErr
    echo delimited(str"(", separated1(pattern"\w", str(",")), str")")
    assert delimited(str"(", separated1(pattern"\w", str(",")), str")")("(a,b,c)").unwrap.mapIt(it.fragment) == @["a", "b", "c"]
    let parser1 = pattern(".").mapRes(
        proc(it: PResult[Spanned]): PResult[Spanned] =
            result = it
            if it.isOk and it.get == "a":
                result = err(it.getSrc, "a!")
    )
    echo parser1
    assert parser1("a").isErr
    assert parser1("b").isOk
