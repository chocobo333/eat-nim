
import sequtils
import strutils
import strformat
import sugar

import macros
import macroutils
import ast_pattern_matching

import parsers
import spanned


proc debugErr*[O](parser: Parser[O], callback: string -> string): Parser[O] =
    newParser(
        proc(src: Spanned): PResult[O] =
            parser.parse(src).mapErr(callback),
        parser.toString
    )

macro annotate(a: proc): untyped =
    discard
macro ParserDef*(def: untyped, body: untyped): untyped =
    var
        parserid: NimNode
        # inputType: NimNode
        members: seq[NimNode] = def[1..^1].mapIt(newIdentDefs(it[0], it[1]))
        memberid: seq[NimNode] = members.mapIt(it[0])
        inits = newStmtList()
        self = ident"self"
        res = ident"result"
        delayedParsers: seq[(NimNode, NimNode, NimNode)]
        parsers: seq[(NimNode, NimNode)]
        parserids: seq[NimNode]
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
                inits.add Asgn(DotExpr(res, left), right)
                continue
            parsers.add (left, right)
            parserids.add left
        of nnkCall(`left`@nnkIdent, nnkStmtList(nnkAsgn(`ty`, `right`))):
            delayedParsers.add (ty, left, right)
            parserids.add left
    result = newStmtList()
    result.add TypeSection(TypeDef(
        parserid, newEmptyNode(),
        RefTy(ObjectTy(
            newEmptyNode(), newEmptyNode(),
            RecList(members)
        ))
    ))
    result.add ProcDef(
        ident(fmt"new{parserid.strVal}"),
        newEmptyNode(),
        FormalParams(parserid),
        newEmptyNode(),
        newStmtList(
            quote do:
                `res` = `parserid`()
        ).add inits
    )
    let
        parser: NimNode = bindSym"Parser"
        debugErr = bindSym"debugErr"
    proc replace(a: NimNode): NimNode =
        a.forNode(
            nnkIdent,
            it => (
                if it in parserids & members.mapIt(it[0]):
                    DotExpr(self, it)
                else:
                    it
            )
        )
    result.add delayedParsers.mapIt(
        ProcDef(
            it[1], newEmptyNode(),
            FormalParams(
                nnkBracketExpr.newTree(parser, it[0]),
                newIdentDefs(self, parserid)
            ),
            newEmptyNode(),
            newEmptyNode()
        )
    )
    result.add parsers.mapIt(
        ProcDef(
            it[0], newEmptyNode(),
            FormalParams(
                ident"auto",
                newIdentDefs(self, parserid)
            ),
            newEmptyNode(),
            newStmtList(
                parseExpr("proc d(it: string): string = it & \" in \" & " & it[0].strVal.escape),
                newCall(debugErr, it[1].replace(), ident"d")
            )
        )
    )
    result.add delayedParsers.mapIt(
        ProcDef(
            it[1], newEmptyNode(),
            FormalParams(
                nnkBracketExpr.newTree(parser, it[0]),
                newIdentDefs(self, parserid)
            ),
            newEmptyNode(),
            newStmtList(
                parseExpr("proc d(it: string): string = it & \" in \" & " & it[1].strVal.escape),
                newCall(debugErr, it[2].replace(), ident"d")   
            )
        )
    )
    let ann: NimNode = bindSym"annotate"
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

    ParserDef Parser(indent: int):
        indent = 0

        Id0 = alt(
            p"[_\p{L}\p{Sm}\p{Nl}ー][_\p{L}\p{Sm}\p{N}ー]*",
            -s"`" > p"[_\p{L}\p{Sm}\p{Nl}ー][_\p{L}\p{Sm}\p{N}ー]*" > -s"`" @ (it => it[0])
        )
        Int0 = p"[0-9]+"

        Atom: AstNode = Float \ Int \ Id + !p"."    @ (it => it[0])

        Id = Id0                                    @ (it => newIdNode(it.fragment))
        Int = Int0                                  @ (it => newIntNode(parseInt(it.fragment)))
        Float = alt(
            Int0 > -s"." > Int0                     @ (it => newFloatNode(parseFloat(it[0].fragment & "." & it[1].fragment))),
            Int0 > -s"." > !Id0                     @ (it => newFloatNode(parseFloat(it[0].fragment))),
            -s"." > Int0                            @ (it => newFloatNode(parseFloat("." & it[0].fragment)))
        )
    var
        parser = newParser()
    echo parser.Atom.parse("4").unwrap()