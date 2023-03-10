import unittest

import ore


suite "Test Lexer":
  test "Test simple":
    const input = "{{ 2 }} Hello, world! 2 + 2 = {{ (2 + 2.92) / \"test\" * variable}}"
    var lexer = initLexer(input)
    while not lexer.isFinished():
      echo lexer.state, " ", lexer.getNextToken().humanRepr


suite "Test parser":
  test "Test simple":
    const input = "{{ 1 }}"#"{{ (-1 - 2 * 3 + 4) * 3 / 4 + 5 }}"
    var p = initParser(input)
    var b = p.parseBlock()
    echo b.treeRepr

suite "Test engine":
  test "Test simple":
    const input = "Hello world! {{ 2 + 2 }}"
    var e = initOreEngine()
    echo e.renderString(input)
