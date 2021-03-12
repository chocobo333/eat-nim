
import strformat
import options
import sugar

import macros

import spanned


type
    RK = enum
        Ok
        Err
    PResult*[O] = object
        src*: Spanned # remained
        case kind: RK
        of Ok:
            ok*: O ##  parsed
        of Err:
            err*: string
    ParseProc*[O] = proc(src: Spanned): PResult[O]
    Parser*[O] = ref object
        parse*: ParseProc[O]
        dropped*: bool
        toString*: string

proc `$`*[O](self: PResult[O]): string =
    case self.kind
    of Ok:
        fmt"Ok{self.ok}"
    of Err:
        fmt"Err(""{self.err}"")"

proc `$`*[O](self: Parser[O]): string =
    self.toString

proc `==`*[O](self, other: PResult[O]): bool =
    if self.kind == other.kind:
        if self.kind == Ok:
            self.ok == other.ok
        else:
            self.err == other.err
    else:
        false

proc overwrite*[O](self: Parser[O], s: string): Parser[O] =
    self.toString = s
    self

proc newParser*[O](parse: ParseProc[O], tos: string, dropped: bool = false): Parser[O] =
    Parser[O](parse: parse, dropped: dropped, toString: tos)
    # Parser[O](parse: parse, dropped: dropped)

proc ok*[O](typ: typedesc[PResult[O]], src: Spanned, val: O): PResult[O] =
    PResult[O](kind: Ok, src: src, ok: val)

template ok*(src: Spanned, val: typed): untyped =
    ok(typeof(result), src, val)

proc ok*[O](typ: typedesc[PResult[O]], val: (Spanned, O)): PResult[O] =
    PResult[O](kind: Ok, src: val[0], ok: val[1])

template ok*(val: typed): untyped =
    ok(typeof(result), val)

proc err*[O](cls: typedesc[PResult[O]], src: Spanned, msg: string): PResult[O] =
    PResult[O](kind: Err, src: src, err: msg)

template err*(src: Spanned, msg: typed): untyped =
    err(typeof(result), src, msg)

proc isOk*[O](val: PResult[O]): bool =
    val.kind == Ok

proc isErr*[O](val: PResult[O]): bool =
    val.kind == Err

proc getSrc*[O](self: PResult[O]): Spanned =
    # returns remained source
    self.src
proc get*[O](self: PResult[O]): O =
    self.ok

proc getErr*[O](self: PResult[O]): string =
    self.err

proc map*[T, O](val: Option[T], callback: T -> (Spanned, O), src: Spanned, msg: string): PResult[O] =
    let a = callback
    if val.isSome:
        let tmp = val.get.callback
        ok tmp[0], tmp[1]
    else:
        err src, msg

macro mapIt*[T](val: Option[T], src: Spanned, itexpr: untyped, msg: string): untyped =
    let it = ident"it"
    quote do:
        `val`.map(
            proc(`it`: auto): auto =
                `itexpr`,
            `src`,
            `msg`
        )

proc map*[O1, O2](self: PResult[O1], callback: O1 -> O2): PResult[O2] =
    if self.isOk:
        ok self.src, self.ok.callback
    else:
        err self.src, self.getErr

proc mapSrc*[O](self: PResult[O], callback: Spanned -> Spanned): PResult[O] =
    if self.isOk:
        ok self.src.callback, self.ok
    else:
        self

proc mapErr*[O](self: PResult[O], callback: string -> string): PResult[O] =
    if self.isOk:
        self
    else:
        err self.src, self.err.callback

proc unwrap*[O](self: PResult[O]): O =
    if self.isOk:
        return self.ok
    else:
        raise newException(ValueError, $self.err)