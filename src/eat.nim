
import eat/[
    patterns,
    repeats,
    combinator,
    spaces,
    lookaheads,
    recovers,
    drop,
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
    drop,
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
    if a.dropped:
        tupl(a, b).map(it => @[it[1]])
    else:
        tupl(a, b).map(it => it[0] & it[1])
proc `@`*[O1, O2](a: Parser[O1], callback: O1 -> O2): Parser[O2] {.inline.} =
    a.map(callback)
proc `@@`*[O1, O2](a: Parser[O1], callback: PResult[O1] -> PResult[O2]): Parser[O2] {.inline.} =
    a.mapRes(callback)
proc `?`*[O](parser: Parser[O]): Parser[Option[O]] {.inline.} =
    opt(parser)
proc `-`*[O](a: Parser[O]): Parser[O] {.inline.} =
    a.drop
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
    



when isMainModule:
    echo Parser[string] is To[string]
    proc add(a: var string, b: Spanned) = a.add b.fragment
    let
        parser = s"e" \ p".." \ p"..."
        parser2 = s"ff" + p".."
        parser3 = s"ff" > -p".." > p"..." @ (it => (result = "";for e in it: result.add e))
        parser4 = p"[a-z]" ^+ -s","
    echo parser
    echo parser2
    echo parser3
    echo parser4
    echo parser.parse("ffdefabc")
    echo parser2.parse("ffdefabc")
    echo parser3.parse("ffdefabc")
    echo separated1(str"a", str(",").drop).parse("a,a,a,a")
    echo parser4.parse("a,b,c")
    echo (s"a" * 5)
    echo (s"a" * 5).parse("aaaaaaa")
    echo (s"a" * 5).parse("aabaaaa")
    echo (*s"a").parse("aaaaaaa")
    echo (*s"b").parse("aaaaaaa")
    echo (+s"b")
    echo (+s"b").parse("aaaaaaa")
    echo delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n.fragment)), s")")
    echo s"ff" ^ spaces(0)
    echo (s"ff" ^ spaces(0)).parse("  ff")
    echo (s"ff" + -s"ee").parse("ffee")

    let
        parser5 = s"e" \ p".." \ p"..."
        parser6 = s"ff" + p".."
        parser7 = s"ff" > -p".." > p"..." @ ((it: seq[Spanned]) => (result = it[0];for e in it[1..^1]: result.add e.fragment))
        parser8 = p"[a-z]" ^+ s","
    echo parser5
    echo parser6
    echo parser7
    echo parser8
    echo parser5.parse("ffdefabc")
    echo parser6.parse("ffdefabc")
    echo parser7.parse("ffdefabc")
    echo separated1(s"a", s(",")).parse("a,a,a,a")
    echo parser8.parse("a,b,c")
    echo (s"a" * 5)
    echo (s"a" * 5).parse("aaaaaaa")
    echo (s"a" * 5).parse("aabaaaa")
    echo (*s"a").parse("aaaaaaa")
    echo (*s"b").parse("aaaaaaa")
    echo (+s"b")
    echo (+s"b").parse("aaaaaaa")
    echo delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n.fragment)), s")")
    echo delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n.fragment)), s")").parse("(12345)")