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
        check str"aiueo".parse("aiueo").isOk
        check str"aiueo".parse("aiu").isErr
        check str"aiueo".parse("biueo").isErr
    test "pattern":
        check pattern".".parse("a").isOk
        check pattern"a.c.".parse("abc®").isOk
    test "sugar":
        check s"aiueo".parse("aiueo").isOk
        check s"aiueo".parse("aiu").isErr
        check s"aiueo".parse("biueo").isErr
        check p".".parse("a").isOk
        check p"a.c.".parse("abc®").isOk

suite "repeats":
    test "many0":
        check many0(s"a").parse("aaaa").isOk
        check many0(s"b").parse("aaaa").isOk
    test "many1":
        check many1(s"a").parse("aaaa").isOk
        check many1(s"b").parse("aaaa").isErr
    test "manyMN":
        discard
    test "times":
        check times(s"a", 3).parse("aaa").isOk
        check times(s"a", 3).parse("aaaa").isOk
        check times(s"a", 3).parse("aa").isErr
    test "fold0":
        check delimited(s"(", p"[0-9]".fold0(0, (acc, n)=>acc+parseInt(n.fragment)), s")").parse("(12345)").unwrap == 15
        check delimited(s"(", p"[0-9]".fold0(1, (acc, n)=>acc*parseInt(n.fragment)), s")").parse("(12345)").unwrap == 120
    test "sugar":
        check (*s"a").parse("aaaa").isOk
        check (*s"b").parse("aaaa").isOk
        check (+(s"a")).parse("aaaa").isOk
        check (+(s"b")).parse("aaaa").isErr
        check (s"a" * 3).parse("aaa").isOk
        check (s"a" * 3).parse("aaaa").isOk
        check (s"a" * 3).parse("aa").isErr

