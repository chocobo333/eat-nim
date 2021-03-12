
import parsers
import utils


proc drop*[O](parser: Parser[O]): Parser[O] =
    parser.dropped = true
    return parser.overwrite(genGraph("Drop", parser))