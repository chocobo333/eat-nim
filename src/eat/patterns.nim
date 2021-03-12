
import strformat
import strutils
import options
import unicode
import sugar

import nre

import parsers
import optionmaps
import utils

import spanned


# proc taken*(self: string, val: string): Option[(string, string)] =
#     if self.startsWith(val):
#         return some (self[val.len..^1], val)
#     else:
#         none (string, string)
# proc takeMatch*(src: string, pattern: string): PResult[string] =
#     let
#         p = re(fmt"(*UTF8){pattern}")
#     match(src, p).filterIt(it.match != "").mapIt((it.rest, it.match), fmt"NotMatch @ pattern""{pattern}""")

proc startsWith(self: Spanned, val: string): bool = self.fragment.startsWith(val)

proc taken(self: Spanned, val: string): Option[(Spanned, Spanned)] =
    if self.startsWith(val):
        let
            n = val.len
            lines = splitLines(val)
            newpos = if lines.len == 1:
                newPosition(self.pos.line, self.pos.col + lines[0].runeLen)
            else:
                newPosition(self.pos.line+1, lines[^1].runeLen + 1)
        some (
            newSpanned(self.fragment[n..^1], newpos, self.endpos),
            newSpanned(self.fragment[0..<n], self.pos, newpos)
        )
    else:
        none (Spanned, Spanned)

type
    RegexMatchSpanned = object
        res: RegexMatch
        pos: Position
        endpos: Position

proc newRegexMatchSpanned(res: Option[RegexMatch], location: Position, endloc: Position): Option[RegexMatchSpanned] =
    if res.isSome:
        some RegexMatchSpanned(res: res.get, pos: location, endpos: endloc)
    else:
        none RegexMatchSpanned

proc match(src: Spanned, pattern: Regex): Option[RegexMatchSpanned] =
    newRegexMatchSpanned(
        match(src.fragment, pattern),
        src.pos,
        src.endpos
    )

proc match(src: RegexMatchSpanned): Spanned =
    newSpanned(src.res.match, src.pos, src.endpos)

proc rest(src: RegexMatchSpanned): Spanned =
    newSpanned(src.res.str[src.res.matchBounds.b+1..^1], src.pos, src.endpos)

proc restnMatch(src: RegexMatchSpanned): (Spanned, Spanned) =
    let
        s = src.res.str
        m = src.res.match
        lines = splitLines(m)
        newpos = if lines.len == 1:
            newPosition(src.pos.line, src.pos.col + lines[0].runeLen)
        else:
            newPosition(src.pos.line+1, lines[^1].runeLen + 1)
    (
        newSpanned(s[src.res.matchBounds.b+1..^1], newpos, src.pos),
        newSpanned(m, src.pos, newpos)
    )

proc takeMatch(src: Spanned, pattern: string): PResult[Spanned] =
    let
        p = re(fmt"(*UTF8){pattern}")
    match(src, p).filterIt(it.match.fragment != "").map(it => it.restnMatch, src, fmt"got ""{src.fragment[0..min(src.fragment.len-1, 8)]}"", but expect for pattern""{pattern}""")

proc str*(bytes: string): Parser[Spanned] =
    newParser(
        proc(src: Spanned): PResult[Spanned] =
            let tmp = src.taken(bytes)
            if tmp.isSome:
                ok tmp.get
            else:
                err(src, &"NotMatch @ str\"{bytes}\""),
        genGraph("Bytes", bytes.escape)
    )

proc pattern*(pattern: string): Parser[Spanned] =
    newParser(
        proc(src: Spanned): PResult[Spanned] =
            # TODO: mixin or bind?
            src.takeMatch(pattern),
        genGraph("Pattern", pattern.escape)
        # "Pattern " & pattern.escape
    )


when isMainModule:
    echo str("ff")
    assert str("ff").parse("ffg").unwrap.fragment == "ff"
    assert pattern("a.c.").parse("abc®aiuec").unwrap.fragment == "abc®"
    echo pattern"a.c."
    echo str"ff"
    assert pattern("b*").parse("aaa").isErr