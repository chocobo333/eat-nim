
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
    newParser(
        proc (src: Spanned): PResult[O] =
            for parser in parsers:
                result = parser.parse(src)
                if result.isOk:
                    return,
        genGraphS("Alt", parsers)
    )

proc alt*[O](parsers: varargs[Parser[O]]): Parser[O] =
    alt(@parsers)

proc tupl*[O1, O2](a: Parser[O1], b: Parser[O2]): Parser[(O1, O2)] =
    type
        O = (O1, O2)
    result = newParser(
        proc(src: Spanned): PResult[O] =
            let resultA = a.parse(src)
            if resultA.isOk:
                let
                    resultB = b.parse(resultA.getSrc)
                    resultA = resultA.get
                if resultB.isOk:
                    let
                        src = resultB.getSrc
                        resultB = resultB.get
                    return ok(src, (resultA, resultB))
                else:
                    return err(resultB.getSrc, resultB.getErr)
            else:
                return err(resultA.getSrc, resultA.getErr),
        genGraph("Tuple", a, b)
    )

proc tupl*[O1, O2, O3](tup: (Parser[O1], Parser[O2], Parser[O3])): Parser[(O1, O2, O3)] =
    newParser(
        proc(src: Spanned): PResult[(O1, O2, O3)] =
            var
                res: (O1, O2, O3)
                src = src
            let tmp0 = tup[0].parse(src)
            if tmp0.isErr:
                return err(tmp0.getErr)
            res[0] = tmp0.get[1]
            src = tmp0.get[0]
            
            let tmp1 = tup[1].parse(src)
            if tmp1.isErr:
                return err(tmp1.getErr)
            res[1] = tmp1.get[1]
            src = tmp1.get[0]

            let tmp2 = tup[2].parse(src)
            if tmp2.isErr:
                return err(tmp2.getErr)
            res[2] = tmp0.get[1]
            src = tmp2.get[0]
            return ok (src, res),
        genGraph("Tuple", tup[0], tup[1], tup[2])
    )

proc sequence*[O](parsers: seq[Parser[O]]): Parser[seq[O]] =
    newParser(
        proc(src: Spanned): PResult[seq[O]] =
            var
                res: seq[O] = @[]
                src = src
            for parser in parsers:
                let tmp = parser.parse(src)
                if tmp.isErr:
                    return err(tmp.getSrc, tmp.getErr)
                if not parser.dropped:
                    res.add tmp.get
                src = tmp.getSrc
            return ok (src, res),
        genGraphS("Sequence", parsers)
    )

proc sequence*[O](parsers: varargs[Parser[O]]): Parser[seq[O]] =
    sequence(@parsers)

proc map*[O1, O2](parser: Parser[O1], callback: O1 -> O2): Parser[O2] =
    newParser(
        proc(src: Spanned): PResult[O2] =
            parser.parse(src).map(callback),
        genGraph("Map", parser, ("$1 -> $2" % [$O1, $O2]))
    )

proc mapRes*[O1, O2](parser: Parser[O1], callback: PResult[O1] -> PResult[O2]): Parser[O2] =
    newParser(
        proc(src: Spanned): PResult[O2] =
            parser.parse(src).callback,
        genGraph("MapRes", parser, ("$1 -> $2" % [$PResult[O1], $PResult[O2]]))
    )

proc mapErr*[O, E1, E2](parser: Parser[O], callback: E1 -> E2): Parser[O] =
    newParser(
        proc(src: Spanned): PResult[O] =
            parser.parse(src).mapErr(callback),
        genGraph("MapErr", parser, ("$1 -> $2" % [$E1, $E2]))
    )

proc opt*[O](parser: Parser[O]): Parser[Option[O]] =
    newParser(
        proc(src: Spanned): PResult[Option[O]] =
            let tmp = parser.parse(src)
            if tmp.isOk:
                ok (tmp.getSrc, some tmp.get)
            else:
                ok (src, none(O))
        ,
        genGraph("Opt", parser)
    )
    
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
    #         result = parser.parse(src)
    #         if result.isOk:
    #             let tmp = terminator.parse(result.get[0])
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

when isMainModule:
    import patterns
    import strutils

    let
        a = pattern"a*"
        b = pattern"b*"
    echo alt(@[a, b])
    assert alt(@[a, b]).parse("aaabbb").unwrap.fragment == "aaa"
    assert alt(a, b).parse("aaabbb").unwrap.fragment == "aaa"
    assert alt(a, b).parse("bbbaaabbb").unwrap.fragment == "bbb"
    echo tupl(b, a)
    assert tupl(b, a).parse("bbbaaabbb").unwrap[1].fragment == "aaa"
    echo sequence(@[a, b])
    assert sequence(@[a, b]).parse("bbbaaabbb").isErr
    echo sequence(b, a, b)
    assert sequence(b, a, b).parse("bbbaaabbb").unwrap.mapIt(it.fragment).join() == "bbbaaabbb"
    assert many0(str"b").parse("bbbbb").unwrap.mapIt(it.fragment).join() == "bbbbb"
    assert many0(str"a").parse("bbbbb").unwrap.mapIt(it.fragment).join() == ""
    assert many1(str"a").parse("bbbbb").isErr
    echo many1(str"a")
    assert many1(str"b").parse("bbbbb").unwrap.mapIt(it.fragment).join() == "bbbbb"
    echo sequence(a, b).map(it => it.join)
    assert sequence(a.map(it => it.fragment), b.map(it => it.fragment)).map(it => it.join).parse("aabbaa").unwrap == "aabb"
    echo opt(str"+")
    assert opt(str"+").parse("3").unwrap.isNone
    assert opt(str"+").parse("+3").unwrap.get.fragment == "+"
    echo separated1(str"a", str(","))
    assert separated1(str"a", str(",")).parse("a,a,a,a").isOk
    assert separated1(str"b", str(",")).parse("a,a,a,a").isErr
    echo separated0(str"b", str(","))
    assert separated0(str"b", str(",")).parse("a,a,a,a").isOk
    assert separated0(str"a", str(",")).parse("a,a,a,a").isOk
    echo terminated(pattern"[0-9]+", str";")
    assert terminated(pattern"[0-9]+", str";").parse("123;").isOk
    assert terminated(pattern"[0-9]+", str";").parse("123:").isErr
    assert terminated(pattern"[0-9]+", str";").parse("w123:").isErr
    echo preceded(pattern"[0-9]+", str";")
    assert preceded(pattern"[0-9]+", str";").parse("123;").isOk
    assert preceded(pattern"[0-9]+", str";").parse("abc;").isErr
    assert preceded(pattern"[0-9]+", str";").parse("123:").isErr
    echo delimited(str"(", separated1(pattern"\w", str(",")), str")")
    assert delimited(str"(", separated1(pattern"\w", str(",")), str")").parse("(a,b,c)").isOk
    let parser1 = pattern(".").mapRes(
        proc(it: PResult[Spanned]): PResult[Spanned] =
            result = it
            if it.isOk and it.get == "a":
                result = err(it.getSrc, "a!")
    )
    echo parser1
    assert parser1.parse("a").isErr
    assert parser1.parse("b").isOk
