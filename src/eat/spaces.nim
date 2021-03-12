
import strformat

import spanned
import parsers
import patterns
import nones
from combinator import alt


proc spaces*(min, max: int): Parser[Spanned] =
    newParser(
        proc(src: Spanned): PResult[Spanned] =
            if min == 0:
                alt(pattern(fmt"[[:blank:]]{{1,{max}}}"), none[Spanned]()).parse(src)
            else:
                pattern(fmt"[[:blank:]]{{{min},{max}}}").parse(src),
        fmt"Spaces({min}, {max})"
    )

proc spaces*(min: int): Parser[Spanned] =
    newParser(
        proc(src: Spanned): PResult[Spanned] =
            if min == 0:
                alt(pattern(r"[[:blank:]]{1,}"), none[Spanned]()).parse(src)
            else:
                pattern(fmt"[[:blank:]]{{{min},}}").parse(src),
        fmt"Spaces({min})"
    )

proc spaces*(min, max: ref int): Parser[Spanned] =
    newParser(
        proc(src: Spanned): PResult[Spanned] =
            var
                min = min[]
                max = max[]
            if min == 0:
                alt(pattern(fmt"[[:blank:]]{{1,{max}}}"), none[Spanned]()).parse(src)
            else:
                pattern(fmt"[[:blank:]]{{{min},{max}}}").parse(src),
        fmt"Spaces({min[]}, {max[]})"
    )

proc spaces*(min: ref int): Parser[Spanned] =
    newParser(
        proc(src: Spanned): PResult[Spanned] =
            var min = min[]
            if min == 0:
                alt(pattern(r"[[:blank:]]{1,}"), none[Spanned]()).parse(src)
            else:
                pattern(fmt"[[:blank:]]{{{min},}}").parse(src),
        fmt"Spaces({min[]})"
    )

when isMainModule:
    echo spaces(1, 4).parse("       ff")
    echo spaces(1).parse("       ff")
    echo spaces(1, 1).parse("       ff")
    echo spaces(0, 4)
    echo spaces(0, 4).parse("ff")
    echo spaces(0)
    echo spaces(0).parse("ff")