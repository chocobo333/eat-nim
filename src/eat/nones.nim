
import parsers
import concepts
import spanned


proc empty*(typ: typedesc[string]): string =
    ""
proc empty*[T](typ: typedesc[seq[T]]): seq[T] =
    @[]

proc empty*(self: typedesc[Spanned]): Spanned =
    Spanned(fragment: "", pos: newPosition())


proc none*[O: Emptiable](): Parser[O] =
    result = proc(src: Spanned): PResult[O] =
        if src.enstring:
            return ok("none", O.empty)
        ok(typeof(result), (src, O.empty))


when isMainModule:
    assert string is Emptiable
    assert Spanned is Emptiable
    assert seq[string] is Emptiable