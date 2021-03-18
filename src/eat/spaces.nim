
import strformat

import spanned
import parsers
import patterns
import nones
from combinator import alt


proc spaces*(min, max: int): Parser[Spanned] =
    result = proc(src: Spanned): PResult[Spanned] =
        if src.enstring:
            return ok(fmt"Spaces({min}, {max})", "")
        if min == 0:
            alt(pattern(fmt"[[:blank:]]{{1,{max}}}"), none[Spanned]())(src)
        else:
            pattern(fmt"[[:blank:]]{{{min},{max}}}")(src)

proc spaces*(min: int): Parser[Spanned] =
    result = proc(src: Spanned): PResult[Spanned] =
        if src.enstring:
            return ok(fmt"Spaces({min})", "")
        if min == 0:
            alt(pattern(r"[[:blank:]]{1,}"), none[Spanned]())(src)
        else:
            pattern(fmt"[[:blank:]]{{{min},}}")(src)

proc spaces*(min, max: ref int): Parser[Spanned] =
    result = proc(src: Spanned): PResult[Spanned] =
        if src.enstring:
            return ok(fmt"Spaces({min[]}, {max[]})", "")
        var
            min = min[]
            max = max[]
        if min == 0:
            alt(pattern(fmt"[[:blank:]]{{1,{max}}}"), none[Spanned]())(src)
        else:
            pattern(fmt"[[:blank:]]{{{min},{max}}}")(src)

proc spaces*(min: ref int): Parser[Spanned] =
    result = proc(src: Spanned): PResult[Spanned] =
        if src.enstring:
            return ok(fmt"Spaces({min[]})", "")
        var min = min[]
        if min == 0:
            alt(pattern(r"[[:blank:]]{1,}"), none[Spanned]())(src)
        else:
            pattern(fmt"[[:blank:]]{{{min},}}")(src)

when isMainModule:
    echo spaces(1, 4)("       ff")
    echo spaces(1)("       ff")
    echo spaces(1, 1)("       ff")
    echo spaces(0, 4)
    echo spaces(0, 4)("ff")
    echo spaces(0)
    echo spaces(0)("ff")