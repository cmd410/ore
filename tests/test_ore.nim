import unittest

import ore


const inputSimple = "{% set a = 2 %}2 + 2 = {{ 2 + a }} Did you know?"


suite "Test lexer":
  test "Test Simple":
    var lex = initLexer(inputSimple)
    const result = [
      (tkStr, lexStateText),
      (tkStmtStart, lexStateStmt),
      (tkVar, lexStateStmt),
      (tkVar, lexStateStmt),
      (tkOperator, lexStateStmt),
      (tkInt, lexStateStmt),
      (tkStmtEnd, lexStateText),
      (tkStr, lexStateText),
      (tkExprStart, lexStateExpr),
      (tkInt, lexStateExpr),
      (tkOperator, lexStateExpr),
      (tkVar, lexStateExpr),
      (tkExprEnd, lexStateText),
      (tkStr, lexStateText)
    ]
    var i = 0
    while not lex.isFinished():
      let tok = lex.getNextToken()
      let o = (tok.kind, lex.state)
      check o == result[i]
      i += 1


suite "Test engine":
  test "Test simple":
    var e = initOreEngine()
    check e.renderString(inputSimple) == "2 + 2 = 4 Did you know?"
  
  test "Test set variable":
    const input = "{% set a = 10 %}{% set b = 32 %}{{ a + b }}"
    var e = initOreEngine()
    check e.renderString(input) == "42"

  test "Test blocks":
    const input = "{% block TestBlock %}Test{% endblock %}"
    var e = initOreEngine()
    check e.renderString(input) == "Test"

  test "Test conditinal blocks":
    const input = "Now we can tell what is {% if true %}True{% else %}False{% endif %} and what is {% if false %}True{% elif false %}Impossible{% else %}False{% endif %}"
    var e = initOreEngine()
    check e.renderString(input) == "Now we can tell what is True and what is False"

  test "Test file":
    var e = initOreEngine()
    check e.renderFile("tests/templates/simple.ore") == "Hello, world!"

  test "Test extends":
    var e = initOreEngine()
    check e.renderFile("tests/templates/extends.ore") == "Hello, world!\n2 + 2 = 4"

  test "Block override":
    var e = initOreEngine()
    check e.renderFile("tests/templates/blockOverride.ore") == "Hello, Ore!\n"
