
import sugar

import spanned
import parsers
import nones
import concepts
import utils


proc lookahead*[O](parser: Parser[O]): Parser[O] =
    newParser(
        proc(src: Spanned): PResult[O] =
            parser.parse(src).mapSrc(it => src),
        genGraph("LookAhead", parser),
        true
    )

proc la*[O](parser: Parser[O]): Parser[O] {.inline.} = lookahead(parser)

proc negate*[O: Emptiable](parser: Parser[O]): Parser[O] =
    newParser(
        proc(src: Spanned): PResult[O] =
            if parser.parse(src).isOk:
                return err(src, "failed negative lookahead check")
            else:
                return ok (src, O.empty),
        genGraph("Negate", parser),
        true
    )


when isMainModule:
    import patterns
    from combinator import sequence
    assert sequence(lookahead(str"a"), pattern"..").parse("ab").isOk
    echo sequence(la(str"a"), pattern"..")
    assert sequence(la(str"a"), pattern"..").parse("ba").isErr
    echo sequence(negate(str"a"), pattern"..")
    assert sequence(negate(str"a"), pattern"..").parse("ba").isOk
    assert sequence(negate(str"b"), pattern"..").parse("ba").isErr
