
import sugar

import spanned
import parsers
import nones
import concepts
import utils


proc lookahead*[O](parser: Parser[O]): Parser[O] =
    result = proc(src: Spanned): PResult[O] =
        if src.enstring:
            return ok(genGraph("LookAhead", parser), O.default)
        parser(src).mapSrc(it => src)

proc la*[O](parser: Parser[O]): Parser[O] {.inline.} = lookahead(parser)

proc negate*[O: Emptiable](parser: Parser[O]): Parser[O] =
    result = proc(src: Spanned): PResult[O] =
        if src.enstring:
            return ok(genGraph("Negate", parser), O.default)
        if parser(src).isOk:
            return err(src, "failed negative lookahead check for " & $parser)
        else:
            return ok (src, O.empty)

proc lookahead*[T, O](parser: IParser[T, O]): IParser[T, O] =
    result = proc(info: T, src: Spanned): PResult[O] =
        if src.enstring:
            return ok(genGraph("LookAhead", parser), O.default)
        info.parser(src).mapSrc(it => src)

proc la*[T, O](parser: IParser[T, O]): IParser[T, O] {.inline.} = lookahead(parser)

proc negate*[O: Emptiable, T](parser: IParser[T, O]): IParser[T, O] =
    result = proc(info: T, src: Spanned): PResult[O] =
        if src.enstring:
            return ok(genGraph("Negate", parser), O.default)
        if info.parser(src).isOk:
            return err(src, "failed negative lookahead check for " & $parser)
        else:
            return ok (src, O.empty)


when isMainModule:
    import patterns
    from combinator import sequence
    assert sequence(lookahead(str"a"), pattern"..")("ab").isOk
    echo sequence(la(str"a"), pattern"..")
    assert sequence(la(str"a"), pattern"..")("ba").isErr
    echo sequence(negate(str"a"), pattern"..")
    assert sequence(negate(str"a"), pattern"..")("ba").isOk
    assert sequence(negate(str"b"), pattern"..")("ba").isErr
