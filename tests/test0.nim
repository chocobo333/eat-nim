# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import strutils
import sugar

import eat


suite "patterns":
    test "str":
        check str"aiueo"("aiueo").isOk
        check str"aiueo"("aiu").isErr
        check str"aiueo"("biueo").isErr
    test "pattern":
        check pattern"."("a").isOk
        check pattern"a.c."("abc®").isOk
    test "sugar":
        check s"aiueo"("aiueo").isOk
        check s"aiueo"("aiu").isErr
        check s"aiueo"("biueo").isErr
        check p"."("a").isOk
        check p"a.c."("abc®").isOk

suite "repeats":
    test "many0":
        check many0(s"a")("aaaa").isOk
        check many0(s"b")("aaaa").isOk
    test "many1":
        check many1(s"a")("aaaa").isOk
        check many1(s"b")("aaaa").isErr
    test "manyMN":
        discard
    test "times":
        check times(s"a", 3)("aaa").isOk
        check times(s"a", 3)("aaaa").isOk
        check times(s"a", 3)("aa").isErr
    test "fold0":
        check delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n.fragment)), s")")("(12345)").unwrap == 15
        check delimited(s"(", p"[0-9]".fold0(1, (acc, n)=>acc*parseInt(n.fragment)), s")")("(12345)").unwrap == 120
    test "sugar":
        check (*s"a")("aaaa").isOk
        check (*s"b")("aaaa").isOk
        check (+(s"a"))("aaaa").isOk
        check (+(s"b"))("aaaa").isErr
        check (s"a" * 3)("aaa").isOk
        check (s"a" * 3)("aaaa").isOk
        check (s"a" * 3)("aa").isErr

