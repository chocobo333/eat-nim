
import parsers
from combinator import alt
import utils


proc recover*[O](parser: Parser[O], rcvr: Parser[O]): Parser[O] {.inline.} =
    alt(parser, rcvr).overwrite(
        genGraph("Recover", rcvr)
    )