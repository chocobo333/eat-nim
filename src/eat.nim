
import eat/[
    patterns,
    repeats,
    combinator,
    spaces,
    lookaheads,
    recovers,
    nones,
    spanned
]
export
    patterns,
    repeats,
    combinator,
    spaces,
    lookaheads,
    recovers,
    nones,
    spanned


import eat/[
    parsers,
    concepts,
    optionmaps,
]
import
    sugar,
    strutils,
    options

export parsers
export optionmaps

proc s*(bytes: string): Parser[Spanned] = str(bytes)
proc p*(pat: string): Parser[Spanned] = pattern(pat)
proc sp*(min: int): Parser[Spanned] = spaces(min)
proc sp*(min, max: int): Parser[Spanned] = spaces(min, max)
proc pos*(): Parser[Spanned] = nones.none[Spanned]()

proc `\`*[O](a: varargs[Parser[O]]): Parser[O] {.inline.} =
    alt(@a)
proc `+`*[O1, O2](a: Parser[O1], b: Parser[O2]): Parser[(O1, O2)] {.inline.} =
    tupl(a, b)
proc `>`*[O](a, b: Parser[O]): Parser[seq[O]] {.inline.} =
    sequence(@[a, b])
proc `>`*[O](a: Parser[seq[O]], b: Parser[O]): Parser[seq[O]] {.inline.} =
    tupl(a, b).map(it => it[0] & it[1])
proc `@`*[O1, O2](a: Parser[O1], callback: O1 -> O2): Parser[O2] {.inline.} =
    a.map(callback)
proc `@@`*[O1, O2](a: Parser[O1], callback: PResult[O1] -> PResult[O2]): Parser[O2] {.inline.} =
    a.mapRes(callback)
proc `?`*[O](parser: Parser[O]): Parser[Option[O]] {.inline.} =
    opt(parser)
proc `^+`*[O1, O2](parser: Parser[O1], delimiter: Parser[O2]): Parser[seq[O1]] {.inline.} =
    separated1(parser, delimiter)
proc `^*`*[O1, O2](parser: Parser[O1], delimiter: Parser[O2]): Parser[seq[O1]] {.inline.} =
    separated0(parser, delimiter)
proc `&`*[O](parser: Parser[O]): Parser[O] {.inline.} =
    lookahead(parser)
proc `!`*[O: Emptiable](parser: Parser[O]): Parser[O] {.inline.} =
    negate(parser)
proc `\=`*[O](parser: Parser[O], rcvr: Parser[O]): Parser[O] {.inline.} =
    recover(parser, rcvr)
proc `*`*[O](parser: Parser[O]): Parser[seq[O]] {.inline.} =
    many0(parser)
proc `+`*[O](parser: Parser[O]): Parser[seq[O]] {.inline.} =
    many1(parser)
proc `*`*[O](parser: Parser[O], n: int): Parser[seq[O]] {.inline.} =
    times(parser, n)

proc `^`*[O1, O2](inner: Parser[O1], outer: Parser[O2]): Parser[O1] {.inline.} =
    surrounded(outer, inner)

template optAlt*{`\` * a}[O](a: Parser[O]): untyped = `\`(a)

template optTerminate*{`+`(a, `-`(b))}[I, O1, O2, E](a: Parser[O1], b: Parser[O2]): untyped =
    terminated(a, b)

# proc `>>`*[O](a: varargs[Parser[O]]): Parser[I, seq[O], E] =
#     sequence(@a)
# template optSeq*{`>>` * a}[O](a: Parser[O]): untyped = `>>`(a)

proc `-`*[O](self: Parser[O]): Parser[O] =
    self

# TODO: does not work
# template optParser*{-a > b}[O](a, b: Parser[O]): Parser[seq[O]] =
#     preceded(a, b).map(it => @[it])
# template optParser*{-a > b}[O](a: Parser[seq[O]], b: Parser[O]): Parser[seq[O]] =
#     preceded(a, b).map(it => @[it])
# template optParser*{a > -b}[O](a, b: Parser[O]): Parser[seq[O]] =
#     terminated(a, b).map(it => @[it])
# template optParser*{a > -b}[O](a: Parser[seq[O]], b: Parser[O]): Parser[seq[O]] =
#     terminated(a, b)

proc `\`*[T, O](a: varargs[IParser[T, O]]): IParser[T, O] {.inline.} =
    alt(@a)
proc `+`*[T, O1, O2](a: IParser[T, O1], b: IParser[T, O2]): IParser[T, (O1, O2)] {.inline.} =
    tupl(a, b)
proc `+`*[T, O1, O2](a: Parser[O1], b: IParser[T, O2]): IParser[T, (O1, O2)] {.inline.} =
    tupl(a.toIParser(T), b)
proc `+`*[T, O1, O2](a: IParser[T, O1], b: Parser[O2]): IParser[T, (O1, O2)] {.inline.} =
    tupl(a, b.toIParser(T))
proc `>`*[T, O](a, b: IParser[T, O]): IParser[T, seq[O]] {.inline.} =
    sequence(@[a, b])
proc `>`*[T, O](a: Parser[O], b: IParser[T, O]): IParser[T, seq[O]] {.inline.} =
    sequence(@[a.toIParser(T), b])
proc `>`*[T, O](a: IParser[T, O], b: Parser[O]): IParser[T, seq[O]] {.inline.} =
    sequence(@[a, b.toIParser(T)])
proc `>`*[T, O](a: IParser[T, seq[O]], b: IParser[T, O]): IParser[T, seq[O]] {.inline.} =
    tupl(a, b).map(it => it[0] & it[1])
proc `>`*[T, O](a: IParser[T, seq[O]], b: Parser[O]): IParser[T, seq[O]] {.inline.} =
    tupl(a, b.toIParser(T)).map(it => it[0] & it[1])
proc `>`*[T, O](a: Parser[seq[O]], b: IParser[T, O]): IParser[T, seq[O]] {.inline.} =
    tupl(a.toIParser(T), b).map(it => it[0] & it[1])
proc `@`*[T, O1, O2](a: IParser[T, O1], callback: O1 -> O2): IParser[T, O2] {.inline.} =
    a.map(callback)
proc `@@`*[T, O1, O2](a: IParser[T, O1], callback: PResult[O1] -> PResult[O2]): IParser[T, O2] {.inline.} =
    a.mapRes(callback)
proc `?`*[T, O](parser: IParser[T, O]): IParser[T, Option[O]] {.inline.} =
    opt(parser)
proc `^+`*[T, O1, O2](parser: IParser[T, O1], delimiter: IParser[T, O2]): IParser[T, seq[O1]] {.inline.} =
    separated1(parser, delimiter)
proc `^*`*[T, O1, O2](parser: IParser[T, O1], delimiter: IParser[T, O2]): IParser[T, seq[O1]] {.inline.} =
    separated0(parser, delimiter)
proc `&`*[T, O](parser: IParser[T, O]): IParser[T, O] {.inline.} =
    lookahead(parser)
proc `!`*[O: Emptiable, T](parser: IParser[T, O]): IParser[T, O] {.inline.} =
    negate(parser)
proc `\=`*[T, O](parser: IParser[T, O], rcvr: IParser[T, O]): IParser[T, O] {.inline.} =
    recover(parser, rcvr)
proc `*`*[T, O](parser: IParser[T, O]): IParser[T, seq[O]] {.inline.} =
    many0(parser)
proc `+`*[T, O](parser: IParser[T, O]): IParser[T, seq[O]] {.inline.} =
    many1(parser)
proc `*`*[T, O](parser: IParser[T, O], n: int): IParser[T, seq[O]] {.inline.} =
    times(parser, n)

proc `^`*[T, O1, O2](inner: IParser[T, O1], outer: IParser[T, O2]): IParser[T, O1] {.inline.} =
    surrounded(outer, inner)

template optAlt*{`\` * a}[T, O](a: IParser[T, O]): untyped = `\`(a)

template optTerminate*{`+`(a, `-`(b))}[T, I, O1, O2, E](a: IParser[T, O1], b: IParser[T, O2]): untyped =
    terminated(a, b)

# proc `>>`*[O](a: varargs[IParser[T, O]]): IParser[T, I, seq[O], E] =
#     sequence(@a)
# template optSeq*{`>>` * a}[O](a: IParser[T, O]): untyped = `>>`(a)

proc `-`*[T, O](self: IParser[T, O]): IParser[T, O] =
    self

# TODO: does not work
# template optParser*{-a > b}[T, O](a, b: IParser[T, O]): IParser[T, seq[O]] =
#     preceded(a, b).map(it => @[it])
# template optParser*{-a > b}[T, O](a: Parser[O], b: IParser[T, O]): IParser[T, seq[O]] =
#     preceded(a.toIParser(T), b).map(it => @[it])
# template optParser*{-a > b}[T, O](a: IParser[T, O], b: Parser[O]): IParser[T, seq[O]] =
#     preceded(a, b.toIParser(T)).map(it => @[it])
# template optParser*{-a > b}[T, O](a: IParser[T, seq[O]], b: IParser[T, O]): IParser[T, seq[O]] =
#     preceded(a, b).map(it => @[it])
# template optParser*{-a > b}[T, O](a: Parser[seq[O]], b: IParser[T, O]): IParser[T, seq[O]] =
#     preceded(a.toIParser(T), b).map(it => @[it])
# template optParser*{-a > b}[T, O](a: IParser[T, seq[O]], b: Parser[O]): IParser[T, seq[O]] =
#     preceded(a, b.toIParser(T)).map(it => @[it])
# template optParser*{a > -b}[T, O](a, b: IParser[T, O]): IParser[T, seq[O]] =
#     terminated(a, b).map(it => @[it])
# template optParser*{a > -b}[T, O](a: Parser[O], b: IParser[T, O]): IParser[T, seq[O]] =
#     terminated(a.toIParser(T), b).map(it => @[it])
# template optIParser*{a > -b}[T, O](a: IParser[T, O], b: Parser[O]): IParser[T, seq[O]] =
#     terminated(a, b.toIParser(T)).map(it => @[it])
# template optParser*{a > -b}[T, O](a: IParser[T, seq[O]], b: IParser[T, O]): IParser[T, seq[O]] =
#     terminated(a, b)
# template optParser*{a > -b}[T, O](a: Parser[seq[O]], b: IParser[T, O]): IParser[T, seq[O]] =
#     terminated(a.toIParser(T), b)
# template optParser*{a > -b}[T, O](a: IParser[T, seq[O]], b: Parser[O]): IParser[T, seq[O]] =
#     terminated(a, b.toIParser(T))

when isMainModule:
    echo Parser[string] is To[string]
    proc add(a: var string, b: Spanned) = a.add b.fragment
    let
        parser = s"e" \ p".." \ p"..."
        parser2 = s"ff" + p".."
        parser3 = s"ff" > -p".." > p"..." @ (it => (result = "";for e in it: result.add e))
        parser4 = p"[a-z]" ^+ s","
    echo parser
    echo parser2
    echo parser3
    echo parser4
    echo parser("ffdefabc")
    echo parser2("ffdefabc")
    echo parser3("ffdefabc")
    echo separated1(str"a", str(","))("a,a,a,a")
    echo parser4("a,b,c")
    echo (s"a" * 5)
    echo (s"a" * 5)("aaaaaaa")
    echo (s"a" * 5)("aabaaaa")
    echo (*s"a")("aaaaaaa")
    echo (*s"b")("aaaaaaa")
    echo (+s"b")
    echo (+s"b")("aaaaaaa")
    echo delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n.fragment)), s")")
    echo s"ff" ^ spaces(0)
    echo (s"ff" ^ spaces(0))("  ff")
    echo (s"ff" + s"ee")("ffee")

    let
        parser5 = s"e" \ p".." \ p"..."
        parser6 = s"ff" + p".."
        parser7 = s"ff" > -p".." > p"..." @ ((it: seq[Spanned]) => (result = it[0];for e in it[1..^1]: result.add e.fragment))
        parser8 = p"[a-z]" ^+ s","
    echo parser5
    echo parser6
    echo parser7
    echo parser8
    echo parser5("ffdefabc")
    echo parser6("ffdefabc")
    echo parser7("ffdefabc")
    echo separated1(s"a", s(","))("a,a,a,a")
    echo parser8("a,b,c")
    echo (s"a" * 5)
    echo (s"a" * 5)("aaaaaaa")
    echo (s"a" * 5)("aabaaaa")
    echo (*s"a")("aaaaaaa")
    echo (*s"b")("aaaaaaa")
    echo (+s"b")
    echo (+s"b")("aaaaaaa")
    echo delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n.fragment)), s")")
    echo delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n.fragment)), s")")("(12345)")

    import strutils
    import eat/utils
    import sugar

    proc sum(a: seq[int]): int =
        for e in a:
            inc result, e
    proc listp(src: Spanned): PResult[seq[int]]
    var valref = (proc(): auto = p"0|([1-9][0-9]*)".map(it => it.fragment.parseInt) \ listp.overwrite("Listp").map(it => sum it))
    proc listp(src: Spanned): PResult[seq[int]] =
        delimited(s"[", separated1(valref(), s","), s"]").overwrite(genGraph("Listp", delimited(s"[", separated1(valref(), s","), s"]")))(src)
    var valp = valref()
    echo valp
    echo listp
    echo valp("[1,[1,3,[3],3],3]")

    type
        A = object
    var a = A()
    proc listp2(self: A, src: Spanned): PResult[seq[int]]
    # var valref2 = (proc(): auto = p"0|([1-9][0-9]*)".map(it => it.fragment.parseInt).toIParser(A) \ listp2.overwrite("Listp").map(it => sum it))
    # var valp2 = valref2()
    let valp2 = p"0|([1-9][0-9]*)".map(it => it.fragment.parseInt).toIParser(A) \ listp2.overwrite("Listp").map(it => sum it)
    proc listp2(self: A, src: Spanned): PResult[seq[int]] =
        delimited(s"[".toIParser(A), separated1(valp2, s",".toIParser(A)), s"]".toIParser(A)).overwrite(genGraph("Listp", delimited(s"[".toIParser(A), separated1(valp2, s",".toIParser(A)), s"]".toIParser(A))))(self, src)
    echo valp2
    echo listp2
    echo a.valp2("[1,[1,3,[3],3],3]")
    echo a.valp2("3")
    echo a.listp2("[1,[1,3,[3],3],3]")