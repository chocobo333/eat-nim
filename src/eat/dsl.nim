
import sequtils
import strutils
import strformat
import sugar

import macros
import macroutils except Lit
import ast_pattern_matching

import parsers
import spanned


proc debugErr*[O](parser: Parser[O], callback: string -> string): Parser[O] =
    result = proc(src: Spanned): PResult[O] =
        if src.enstring:
            ok($parser, O.default)
        else:
            parser(src).mapErr(callback)

proc d(a: string): string -> string =
    result = proc(it: string): string =
        fmt"{it} in {a}"

macro annotate(a: proc): untyped =
    discard
macro IParserType(typ: typedesc): untyped =
    let t = typ.getTypeInst[1]
    let
        T = t[0][1][1]
        O = t[0][0][1]
        iparser = bindSym"IParser"
    nnkBracketExpr.newTree(iparser, T, O)
macro ParserDef*(def: untyped, body: untyped): untyped =
    var
        parserid: NimNode
        # inputType: NimNode
        members: seq[NimNode] = def[1..^1].mapIt(newIdentDefs(it[0], it[1]))
        memberid: seq[NimNode] = members.mapIt(it[0])
        inits = newStmtList()
        parsers: seq[(NimNode, NimNode, NimNode)] # name, type, def
        delayed: seq[NimNode] # name
        defed: seq[NimNode]
    def.matchAst:
    of nnkObjConstr:
        def[0].matchAst:
        of nnkIdent:
            parserid = def[0]
            # inputType = bindSym"Spanned"
        # of nnkBracketExpr(`id`@nnkIdent, `ty`@nnkIdent):
        #     parserid = id
        #     inputType = ty
    for e in body:
        e.matchAst:
        of nnkAsgn(`left`@nnkIdent, `right`):
            if left in memberid:
                inits.add Asgn(DotExpr(ident"result", left), right)
                continue
            parsers.add (left, ident"auto", right)
        of nnkCall(`left`@nnkIdent, nnkStmtList(nnkAsgn(`ty`, `right`))):
            parsers.add (left, ty, right)

    result = newStmtList()
    var
        before: seq[NimNode]
        after: seq[NimNode]
    let
        self = ident"self"
        res = ident"result"
        src = ident"src"
        parser: NimNode = bindSym"Parser"
        iparser: NimNode = bindSym"IParser"
        presult: NimNode = bindSym"PResult"
        spanned: NimNode = bindSym"Spanned"
        ovrwrite: NimNode = bindSym"overwrite"
        debugErr = bindSym"debugErr"
        iparsertype = bindSym"IParserType"
        d = bindSym"d"
        ann: NimNode = bindSym"annotate"
        parserids = parsers.mapIt(it[0])
    proc replace(a: NimNode, b: NimNode = self): NimNode =
        a.forNode(
            nnkIdent,
            it => (
                if it in memberid:
                    DotExpr(b, it)
                else:
                    it
            )
        )
    proc overwriteDelayed(a: NimNode): NimNode =
        a.forNode(
            nnkIdent,
            proc(it: NimNode): auto =
                if it in delayed:
                    newCall(DotExpr(it, ovrwrite), newStrLitNode it.strVal)
                else:
                    it
        )
    proc toIParserR(a: NimNode): NimNode =
        case a.kind
        of ContainerNodeKinds:
            discard
        else:
            discard

    result.add TypeSection(
        TypeDef(
            parserid,
            Empty(),
            RefTy(
                ObjectTy(
                    Empty(),
                    Empty(),
                    RecList(members)
                )
            )
        )
    )
    result.add ProcDef(
        ident(fmt"new{parserid.strVal}"),
        Empty(),
        FormalParams(parserid),
        Empty(),
        newStmtList(
            quote do:
                `res` = `parserid`()
        ).add inits
    )
    for (name, typ, def) in parsers:
        var
            containparsers: bool = false
            containmembers: bool = false
            containnondefed: bool = false
            containself: bool = false
        def.matchAstRecursive:
        of `it`@nnkIdent:
            if it in memberid:
                containmembers = true
            if it in parserids:
                containparsers = true
                if it notin defed:
                    containnondefed = true
        if not containparsers:
            defed.add name
            let
                typ = if typ.kind == nnkIdent and typ.strVal == "auto":
                    typ
                else:
                    nnkBracketExpr.newTree(iparser, parserid, typ)
                def = def.overwriteDelayed
            if containmembers:
                let
                    name2 = ident(fmt"{name.strVal}'")
                    def = def.replace()
                result.add quote do:
                    proc `name2`(`self`: `parserid`, `src`: `spanned`): `typ` =
                        `def`(`self`, `src`)
                    let `name`: `iparsertype`(typeof `name2`) = `name2`
            else:
                result.add quote do:
                    let `name`: `typ` = (`def`).toIParser(`parserid`)
        else:
            if not containnondefed:
                defed.add name
                let
                    typ = if typ.kind == nnkIdent and typ.strVal == "auto":
                        typ
                    else:
                        nnkBracketExpr.newTree(iparser, parserid, typ)
                    def = def.overwriteDelayed
                if containmembers:
                    let
                        name2 = ident(fmt"{name.strVal}'")
                        def = def.replace()
                    result.add quote do:
                        proc `name2`(`self`: `parserid`, `src`: `spanned`): `typ` =
                            `def`(`self`, `src`)
                        let `name`: `iparsertype`(typeof `name2`) = `name2`
                else:
                    result.add quote do:
                        let `name`: `typ` = (`def`).toIParser(`parserid`)
            else:
                defed.add name
                delayed.add name
                let
                    typ = if typ.kind == nnkIdent and typ.strVal == "auto":
                        typ
                    else:
                        nnkBracketExpr.newTree(presult, typ)
                    def = def.overwriteDelayed
                    defr = def.replace()
                if typ == ident"auto":
                    error "A type annotaion is needed.", name
                result.add quote do:
                    proc `name`(`self`: `parserid`, `src`: `spanned`): `typ`
                after.add quote do:
                    proc `name`(`self`: `parserid`, `src`: `spanned`): `typ` = (`defr`).toIParser(`parserid`)(`self`, `src`)

    result.add after
            
    result.add parserids.mapIt(
        newCall(ann, it)
    )
    echo result.repr


when isMainModule:
    import ../eat
    import utils
    import strutils

    import strformat


    type
        AstKind* = enum
            akInt
            akFloat
            akId

    type
        AstNode* = object
            case kind*: AstKind
            of akInt:
                intVal*: BiggestInt
            of akFloat:
                floatVal*: BiggestFloat
            of akId:
                strVal*: string

    proc `$`*(self: AstNode): string =
        let k = fmt"{($self.kind)[2..^1]}"
        case self.kind:
        of akInt:
            genGraph(k, self.intVal)
        of akFloat:
            genGraph(k, self.floatVal)
        of akId:
            genGraph(k, &"\"{self.strVal}\"")

    proc newIntNode*(val: BiggestInt): AstNode =
        AstNode(kind: akInt, intVal: val)

    proc newFloatNode*(val: BiggestFloat): AstNode =
        AstNode(kind: akFloat, floatVal: val)

    proc newIdNode*(name: string): AstNode =
        AstNode(kind: akId, strVal: name)

    proc sum(a: seq[int]): int =
        for e in a:
            inc result, e

    ParserDef Parser(indent: seq[int]):
        indent = @[0]

        Id0 = alt(
            p"[_\p{L}\p{Sm}\p{Nl}ー][_\p{L}\p{Sm}\p{N}ー]*",
            -s"`" > p"[_\p{L}\p{Sm}\p{Nl}ー][_\p{L}\p{Sm}\p{N}ー]*" > -s"`" @ (it => it[0])
        )
        Int0 = p"[0-9]+"

        Atom: AstNode = Float \ Int \ Id

        Id = Id0                                    @ (it => newIdNode(it.fragment))
        Int = Int0                                  @ (it => newIntNode(parseInt(it.fragment)))
        Float = alt(
            Int0 > s"." > Int0                     @ (it => newFloatNode(parseFloat(it[0].fragment & "." & it[2].fragment))),
            Int0 > s"." > !Id0                     @ (it => newFloatNode(parseFloat(it[0].fragment))),
            s"." > Int0                            @ (it => newFloatNode(parseFloat("." & it[1].fragment)))
        )
        Test: string = alt(
            s"0" + Test                             @ (it => it[0].fragment & it[1]),
            s"0".toIParser(Parser)                  @ (it => it.fragment)
        )
        Val: int = alt(
            p"0|([1-9][0-9]*)".toIParser(Parser)    @ (it => it.fragment.parseInt),
            List                                    @ (it => sum it)
        )
        List = delimited(
            s"[".toIParser(Parser),
            separated1(Val, s",".toIParser(Parser)),
            s"]".toIParser(Parser)
        )
    var
        parser = newParser()
    echo parser.Int0("3").unwrap()
    echo parser.Id0("a").unwrap()
    echo parser.Float("2.4").unwrap()
    echo parser.Float(".4").unwrap()
    echo parser.Atom(".4").unwrap()

    echo parser.Test("000")
    echo parser.Val("[4,3,[3,[1,2]]]")
    echo parser.List("[4,3,[3,[1,2]]]")
