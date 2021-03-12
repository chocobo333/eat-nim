
import strformat
import strutils


type
    Position* = object
        line*: int
        col*: int
    Spanned* = object
        fragment*: string
        pos*: Position
        endpos*: Position

proc newPosition*(line: int = 1, col : int = 1): Position =
    Position(line: line, col: col)

proc `$`*(self: Position): string =
    fmt"({self.line}, {self.col})"

proc newSpanned*(frag: string, loc: Position, endloc: Position): Spanned =
    Spanned(fragment: frag, pos: loc, endpos: endloc)

converter toSpanned*(s: string): Spanned =
    let
        lines = splitLines(s)
    newSpanned(s, newPosition(), newPosition(lines.len, lines[^1].len+1))

proc `$`*(self: Spanned): string =
    &"\"{self.fragment}\"{self.pos}"

proc add*(self: var Spanned, s: string) =
    self.fragment.add s
