# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

## **Ore** is a runtime capable string templating engine for nim language.
## 
## Template syntax looks like this:
## ```jinja
## # insert expression
## 2 + 2 = {{ 2 + 2 * a }}
## 
## # define variables
## {% set variable = 42 %}
## 
## # Template inheritance
## {% extend "path/to/other.ore" %}  
## 
## # named blocks
## {% block name %}   
## {% endblock %}
## 
## # conditional blocks
## {% if condition %}
## {% elif other_condition %}
## {% else %}
## {% endif %}
## 
## # for loop blocks (WIP)
## {% for i in iterator %}
## {% endfor %}
## 
## # while loop blocks (WIP)
## {% while condition %}
## {% endwhile %}
## 
## # Template composition (WIP)
## {% insert "path/to/other.ore" arg1=1 arg2="hello" %}
## 
## # Space control
## this {>}
## is one
## {<} line
## ```
runnableExamples:
  var ctx = initOreContext()

  ctx.defines:
    # assign variable `a`
    a = 42

  echo ctx.renderString("Quick math! 2 + 2 = {{ 2 + 2 }}")
  echo ctx.renderFile("tests/templates/simple.ore")


import strformat
import strutils
import math
import tables
import macros
import options
import os
import sugar


type
  CodePos = tuple[offset, line, col: int]

  TokenKind = enum
    tkEof
    
    tkOperator
    
    tkStr, tkInt, tkFloat,
    tkBool, tkVar

    tkExprStart, tkExprEnd  ## {{ and }}
    tkStmtStart, tkStmtEnd  ## {% and %}
    tkBracket, tkComma
  
  OperatorKind = enum
    opDot,
    opPlus, opMinus
    opMult, opDivide
    opAmp, # &
    # =     ==        <        >         <=        >=
    opEq, opCmpEq, opCmpLs, opCmpGt, opCmpEqLs, opCmpEqGt

    # and   or    xor
    opAnd, opOr, opXor,
    # in   notin
    opIn, opNotIn,
    # not
    opNot

  BracketKind = enum
    brLParen, brRParen,
    brLBrack, brRBrack

  Token = object
    pos: CodePos
    
    case kind*: TokenKind
    of tkOperator:
      opKind*: OperatorKind
    of tkStr, tkVar:
      strValue*: string
      quoted*: bool
    of tkInt:
      intValue*: int
    of tkFloat:
      floatValue*: float
    of tkBool:
      boolValue*: bool
    of tkBracket:
      brKind*: BracketKind
    else: discard
  
  OreError = object of CatchableError



# Enum to string conversions
const brKindToStr: array[BracketKind, string] = ["(", ")", "[", "]"]
const opKindToStr: array[OperatorKind, string] = [".", "+", "-", "*", "/", "&", "=", "==", "<", ">", "<=", ">=", "and", "or", "xor", "in", "notin", "not"]
const opToPrecedence: array[OperatorKind, int] = [ 10,  8,   8,   9,   9,   7 ,  1,   5,    5,   5,   5,    5  ,   4,    3,     3  ,   5,    5    ,   0  ]


func initToken(pos: CodePos, kind: static[TokenKind]): Token =
  ## Create valueless token, only accepts valueless token kinds
  when kind notin {tkEof, tkExprStart, tkExprEnd, tkStmtStart, tkStmtEnd, tkComma}:
    {.error: "Only valueless token kinds are acceptable in this constructor".}
  Token(
    pos: pos,
    kind: kind
  )

func initToken(pos: CodePos, brKind: BracketKind): Token =
  ## Create bracket token
  Token(
    pos: pos,
    kind: tkBracket,
    brKind: brKind
  )

func initToken(pos: CodePos, opKind: OperatorKind): Token =
  ## Create opearator token
  Token(
    pos: pos,
    kind: tkOperator,
    opKind: opKind
  )

func initToken(pos: CodePos, value: string, quoted: bool, kind: static[TokenKind] = tkStr): Token =
  ## Create token from string value given.
  ## Valid kinds are tkStr and tkVar
  when kind notin {tkStr, tkVar}:
    {.error: "string token kind must be tkStr or tkVar".}
  Token(
    pos: pos,
    kind: kind,
    strValue: value,
    quoted: quoted
  )

func initToken(pos: CodePos, value: int): Token =
  ## Create token from integer value
  Token(
    pos: pos,
    kind: tkInt,
    intValue: value
  )

func initToken(pos: CodePos, value: float): Token =
  ## Create token from float value
  Token(
    pos: pos,
    kind: tkFloat,
    floatValue: value
  )

func initToken(pos: CodePos, value: bool): Token =
  ## Create token from boolean value
  Token(
    pos: pos,
    kind: tkBool,
    boolValue: value
  )

func `$`*(tk: Token): string =
  ## Transform token into a string that could have produced it
  case tk.kind
  of tkEof: result = ""
  of tkStr:
    if tk.quoted:
      result = "\"" & tk.strValue & "\""
    else:
      result = tk.strValue
  of tkInt:
    result = $tk.intValue
  of tkFloat:
    result = $tk.floatValue
  of tkBool:
    result = $tk.boolValue
  of tkVar:
    result = tk.strValue
  of tkOperator:
    result = opKindToStr[tk.opKind]
  of tkBracket:
    result = brKindToStr[tk.brKind]
  of tkExprStart:
    result = "{{"
  of tkExprEnd:
    result = "}}"
  of tkStmtStart:
    result = "{%"
  of tkStmtEnd:
    result = "%}"
  of tkComma:
    result = ","

func humanRepr(pos: CodePos): string =
  if pos.offset < 0:
    "unknown location"
  else:
    fmt"line {pos.line}, col {pos.col} (offset: {pos.offset})"

func humanRepr(tk: Token): string =
  ## Get descriptive, yet human-readable string representation of token
  result = "Token: "
  case tk.kind
  of tkEof: result &= "<End of File> "
  of tkOperator:
    result &= fmt"""operator "{opKindToStr[tk.opKind]}" ({tk.opKind}) """
  of tkStr:
    if tk.quoted: result &= "quoted"
    else: result &= "unquoted"
    result &= fmt""" string "{tk.strValue}" """
  of tkInt:
    result &= fmt"integer {tk.intValue} "
  of tkFloat:
    result &= fmt"float {tk.floatValue} "
  of tkBool:
    result &= fmt"bool {tk.boolValue} "
  of tkVar:
    result &= fmt"identifier {tk.strValue} "
  of tkExprStart:
    result &= "expr start \"{{\" "
  of tkExprEnd:
    result &= "expr end \"}}\" "
  of tkStmtStart:
    result &= "stmt start \"{%\" "
  of tkStmtEnd:
    result &= "stmt end \"%}\" "
  of tkBracket:
    result &= fmt"""bracket "{brKindToStr[tk.brKind]}" """
  of tkComma:
    result = "comma \",\""
  
  result &= fmt"at {tk.pos.humanRepr}"


type
  LexState = enum
    lexStateText
    lexStateExpr
    lexStateStmt

  Lexer = object
    ## Lexer - the most low-level part of this machine.
    ## It's only job is to turn text input into parser-digestable tokens
    pos*: CodePos
    state*: LexState
      ## Lexer can be in one state at a time.
      ## State changes how tokens are produced in `getNextToken`
      ## 
      ## - `lexStateText` - assumes all text encountered until expr delimiter is unquoted string
      ## - `lexStateExpr` and `lexStateStmt` - parse code inside expression and statement blocks.
      ##    Ignores whitespaces.
    input*: string
    cChar*: char
    exprPos*: Option[int]
      ## Lexer postion inside expression. Not used for much.
    defState: Option[LexState]
      ## Defered state is a state that should be applied to lexer on next `getNextToken` call.
      ## When in encounters with expr delimiters


proc advance(lex: var Lexer): char {.discardable.} =
  ## Advance lexer by one char
  lex.pos.offset += 1

  if lex.exprPos.isSome:
    lex.exprPos.get += 1

  case lex.cChar
  of '\n':
    lex.pos.line += 1
    lex.pos.col = 1
  else:
    lex.pos.col += 1

  if lex.pos.offset > lex.input.high:
    lex.cChar = '\0'
  else:
    lex.cChar = lex.input[lex.pos.offset]

  return lex.cChar


func initLexer(input: string): Lexer =
  ## Create lexer for given text
  result = Lexer(
    pos: (-1, 1, 0),
    state: lexStateText,
    input: input,
    cChar: '\0',
    exprPos: none[int](),
    defState: none[LexState]()
  )
  result.advance()


func peek(lex: Lexer, offset: int = 1): char =
  ## Return char that is offset characters away from current
  let pos = lex.pos.offset + offset

  if pos > lex.input.high:
    result = '\0'
  else:
    result = lex.input[pos]


proc skipWhitespace(lex: var Lexer): char =
  ## Skip whitespace characters ({' ', '\t', '\n', '\r'})
  while lex.cChar in {' ', '\t', '\n', '\r'}:
    lex.advance
  result = lex.cChar


proc parseNum(lex: var Lexer): Token =
  ## Parse number. Be it integer or float

  # Behold, most efficient number parsing
  let pos = lex.pos
  var
    c = lex.cChar
    numInt = (c.byte - '0'.byte).int
    next = lex.peek()

  while next.isDigit:
    lex.advance()
    c = lex.cChar
    next = lex.peek()
    numInt = numInt * 10 + (c.byte - '0'.byte).int
  if next == '.':
    next = lex.peek(2)
    var
      exp = 1.0
      numFloat = 0.0
    
    if next.isDigit:
      lex.advance()
      while next.isDigit:
        lex.advance()
        c = lex.cChar
        numFloat = numFloat + (c.byte - '0'.byte).float / pow(10'f, exp)
        exp += 1.0
        next = lex.peek()

      result = pos.initToken(numInt.float + numFloat)
    else:
      result = pos.initToken(numInt)
  else:
    result = pos.initToken(numInt)


func tokenizeExpr(lex: var Lexer): Token =
  ## Get next token of an expression
  var c = lex.skipWhitespace
  let pos = lex.pos

  if lex.exprPos.get() == 0:
    case c
    of '{':
      result = pos.initToken(tkExprStart)
      return
    of '%':
      result = pos.initToken(tkStmtStart)
      return
    else:
      raise OreError.newException:
        "Unexpected error"

  case c
  of '1', '2', '3', '4', '5', '6', '7', '8', '9', '0':
    result = lex.parseNum()
  of '"':
    c = lex.advance()
    var text = ""

    while c != '"':
      case c
      of '\\':
        let esqPos = lex.pos
        c = lex.advance()
        case c
        of 'n': text &= '\n'
        of 'r': text &= '\r'
        of 't': text &= '\t'
        of '"': text &= '"'
        # TODO: \u1234 \x12 stuff
        else:
          raise OreError.newException:
            fmt"Invalid escape sequence at {esqPos.humanRepr}"
      of '\0':
        raise OreError.newException:
          fmt"Unexpected end of file while parsing string at {pos.humanRepr}"
      else:
        text &= c
      c = lex.advance()

    result = pos.initToken(text, true)

  of ',':
    result = pos.initToken(tkComma)
  else:

    template maybeCloseExpr(lex: var Lexer, t: static[TokenKind]): untyped =
      ## Check for closing }, if present - leave expression
      if lex.peek == '}':
        lex.advance()
        lex.state = lexStateText
        lex.exprPos = none[int]()
        return pos.initToken(t)
    
    case c
    of '}':
      lex.maybeCloseExpr(tkExprEnd)
    of '%':
      lex.maybeCloseExpr(tkStmtEnd)
    else: discard

    let doubleOp = c & lex.peek()
    var opIdx = opKindToStr.find(doubleOp)
    if opIdx != -1:
      lex.advance()
      result = pos.initToken(opIdx.OperatorKind)
      return
    else:
      opIdx = opKindToStr.find($c)
      if opIdx != -1:
        result = pos.initToken(opIdx.OperatorKind)
        return

    let brIdx = brKindToStr.find($c)
    if brIdx != -1:
      result = pos.initToken(brIdx.BracketKind)
      return

    if c.isAlphaAscii():
      var
        ident = $c
        next = lex.peek()

      while next.isAlphaAscii or next in {'_'}:
        c = lex.advance()
        ident &= c
        next = lex.peek()
        
      case ident
      of "true":
        result = pos.initToken(true)
      of "false":
        result = pos.initToken(false)
      else:
        opIdx = opKindToStr.find(ident)
        if opIdx != -1:
          result = pos.initToken(opIdx.OperatorKind)
          return
        result = pos.initToken(ident, false, tkVar)
      return


func getNextToken(lex: var Lexer): Token =
  ## Parse next token
  var c = lex.cChar

  if c == '\0':
    return lex.pos.initToken(tkEof)
  
  lex.state = lex.defState.get(lex.state)
  lex.defState = none[LexState]()

  case lex.state
  of lexStateText:
    let pos = lex.pos
    var
      text = ""
      
    while true:
      case c
      of '{':  # Entering code block?
        case lex.peek
        of '{':  # expression block perhaps?
          lex.defState = lexStateExpr.some
          lex.exprPos = -1.some
          break
        of '%':
          lex.defState = lexStateStmt.some
          lex.exprPos = -1.some
          break
        of '<', '>':
          case lex.peek 2
          of '}':
            c = lex.advance()
            let a = c
            lex.advance()
            c = lex.advance()
            case a
            of '<':
              text = text.strip(leading=false, trailing=true)
              continue
            of '>':
              c = lex.skipWhitespace()
              continue
            else: discard
          else:
            text &= c
        else:  # Nope, no expression here treat as normal char
          text &= c
      of '\0':
        break
      else:
        text &= c
      c = lex.advance()
    result = pos.initToken(text, false)

  of lexStateExpr:
    result = lex.tokenizeExpr()
  of lexStateStmt:
    result = lex.tokenizeExpr()
  lex.advance()


type
  NodeKind = enum
    ndNoOp
    ndValue
    ndRope
    ndUnOp
    ndBinOp
    ndSetVar
    ndIfBlock
    ndExtends
    ndWhile,
    ndList

  Node = ref object
    ## Node is base unit of AST
    ## generated by `Parser`
    precedence*: int
    origin*: Token
    case kind*: NodeKind
    of ndNoOp, ndValue: discard
    of ndRope:
      rope*: seq[Node]
    of ndUnOp:
      operand*: Node
    of ndBinOp:
      left*: Node
      right*: Node
    of ndSetVar:
      varValue: Node
    of ndIfBlock, ndWhile:
      conditionNode*: Node
      truePath*: Node
      falsePath*: Node
    of ndExtends:
      otherNode*: Node
    of ndList:
      items*: seq[Node]
      finished: bool

func getNodePos(n: Node): CodePos =
  ## Attempt to get position in code given node
  ## originates from
  if n == nil: return (-1, -1, -1)
  case n.kind
  of ndRope:
    result = (0, 0, 0)
    if n.rope.len > 0:
      result = n.rope[0].getNodePos()
  else:
    result = n.origin.pos

func treeRepr(n: Node, indent: int = 0): string =
  ## Return a tree string representation of node
  result = indent.spaces & "| "
  if n == nil:
    result &= "nil\n"
    return
  const deltaIndent = 4

  func endl(n: Node): string =
    if not result.endsWith('\n'):
      result = fmt" [ {n.getNodePos().humanRepr} ]" & '\n'
    else: result = ""
  case n.kind
  of ndValue:
    result &= fmt"Value: <{n.origin}> ({n.origin.kind})" & endl(n)
  of ndRope:
    result &= "Rope '" & $n.origin & "' :" & endl(n)
    for i in n.rope:
      result &= i.treeRepr(indent + deltaIndent)
  of ndUnOp:
    result &= fmt"Unary operator ({n.origin}):" & endl(n)
    result &= n.operand.treeRepr(indent + deltaIndent)
  of ndBinOp:
    result &= fmt"Binary operator ({n.origin}):" & endl(n)
    result &= n.left.treeRepr(indent + deltaIndent)
    result &= n.right.treeRepr(indent + deltaIndent)
  of ndNoOp:
    result &= "NoOp"
  of ndSetVar:
    result &= fmt"set {n.origin}:" & endl(n)
    result &= n.varValue.treeRepr(indent + deltaIndent)
  of ndIfBlock:
    result &= "if:\n"
    result &= n.conditionNode.treeRepr(indent + deltaIndent)
    result &= n.truePath.treeRepr(indent + deltaIndent)
    result &= n.falsePath.treeRepr(indent + deltaIndent)
  of ndWhile:
    result &= "while:\n"
    result &= n.conditionNode.treeRepr(indent + deltaIndent)
    result &= n.truePath.treeRepr(indent + deltaIndent)
    result &= n.falsePath.treeRepr(indent + deltaIndent)
  of ndExtends:
    result &= "Extends:\n" & n.otherNode.treeRepr(indent + deltaIndent)
  of ndList:
    result &= "List:\n"
    for i in n.items:
      result &= i.treeRepr(indent + deltaIndent)

func isConst(n: Node): bool =
  ## Check if node can be evaluated
  ## without any external
  result = false
  if n == nil: return
  
  case n.kind
  of ndValue:
    result = n.origin.kind != tkVar
  of ndRope:
    if n.origin.kind == tkVar:
      return false  # Named blocks never constant
    result = true
    for i in n.rope:
      result = result and i.isConst()
      if not result: break
  of ndUnOp:
    result = n.operand.isConst()
  of ndBinOp:
    result = n.left.isConst() and n.right.isConst()
  of ndNoOp:
    result = true
  of ndIfBlock:
    result = n.conditionNode.isConst()
  of ndSetVar: discard
  of ndExtends, ndWhile:
    result = false
  of ndList:
    result = true
    for i in n.items:
      result = result and i.isConst()
      if not result: return

type
  ParseState = enum
    parseStateQuit
    parseStateText
    parseStateExpr
    parseStateStmt

  Parser = object
    ## Parser builds AST
    ## based on lexer-provided tokens
    lex*: Lexer
    cTok*: Token
    state*: ParseState

func initParser(s: string): Parser =
  Parser(
    lex: initLexer(s),
    state: parseStateText
  )

template assertRule(p: Parser, rule: untyped, pos: CodePos, message: string = "unknown"): untyped =
  ## Check if given rule is satisfied
  ## if not raise OreError with a detailed message
  if not rule:
    raise OreError.newException: "Syntax error: " & message & " at " & $pos.humanRepr

proc advance(p: var Parser): Token {.discardable.} =
  p.cTok = p.lex.getNextToken()
  p.cTok

proc eatToken(p: var Parser, kinds: set[TokenKind]): Token {.discardable.} =
  ## Confirm that current token is one of given kinds, and advances the parser
  ## otherwise, raise OreError.
  ## 
  ## Returns the token in question.
  result = p.cTok
  p.assertRule(p.cTok.kind in kinds, result.pos):
    "Unexpected token." & " Expected " & $kinds & ", got " & $result.kind & " instead"
  discard p.advance()

proc eatOperator(p: var Parser, opKinds: set[OperatorKind]): Token {.discardable.} =
  ## Confirm that current token is an operator and one of specific kinds of operators
  ## if not raise OreError.
  result = p.eatToken({tkOperator})
  p.assertRule(result.opKind in opKinds, result.pos):
    "Unexpected operator '" & $result.opKind & "'. Expected " & $opKinds & "."

proc eatBracket(p: var Parser, brKinds: set[BracketKind]): Token {.discardable.} =
  ## Confirm that current token is a bracket and one of specific kinds of brackets
  ## if not raise OreError.
  result = p.eatToken({tkBracket})
  p.assertRule(result.brKind in brKinds, result.pos):
    "Unexpected operator '" & $result.brKind & "'. Expected " & $brKinds & "."

proc unreachable() {.noreturn.} =
  ## Call on unreacahble? branches and other
  ## impossible? condition to catch bugs in parser 
  raise OreError.newException:
    (
      "Invalid syntax." &
      "This is likely a bug in the parser, please report at " &
      "https://github.com/cmd410/ore/issues"
    )

proc parseExpression(p: var Parser): Node =
  # BEHOLD, *mostly* recursionless recursive descent expression parsing
  var tok = p.cTok
  var subExprStack: seq[Node]
    ## This variable contains a stack
    ## of expressions in parsing from 
    ## outer-most to inner-most
  
  proc setRightmostNil(a, b: Node): bool {.discardable.} =
    ## Set deepest empty rightmost operand of node `a`, to node `b`
    ## returns false if no such operand was found, true on success
    result = false
    var current = a
    while true:
      case current.kind
      of ndBinOp:
        if current.right == nil:
          current.right = b     # Fill and leave
          return true
        current = current.right   # descend
      
      of ndUnOp:
        if current.operand == nil:
          current.operand = b     # Fill and leave
          return true
        current = current.operand   # descend
      else: break

  while true:
    case tok.kind
    of tkStr, tkBool, tkFloat, tkInt, tkVar:
      let node = Node(kind: ndValue, origin: tok)
      if result == nil:
        # Put value node in result if nothing was parsed before
        result = node
      else:
        # if something was parsed before we do...
        case result.kind
        of ndUnOp, ndBinOp, ndList:
          p.assertRule(result.setRightmostNil(node), tok.pos):
            "Invalid syntax"
        else: 
          unreachable()

    of tkOperator:
      if result == nil:
        # Nothing was parsed before operator
        # therefore it is unary opeartor
        result = Node(kind: ndUnOp, origin: tok)
      else:
        # Check previous operation
        case result.kind
        of ndValue, ndList:
          # value before operator creates binary operator
          result = Node(kind: ndBinOp, origin: tok, left: result)
        of ndUnOp, ndBinOp:
          # Attempt to fill rightmost with UnaryOp
          if not result.setRightmostNil(Node(kind: ndUnOp, origin: tok)):
            # If all right operands are full
            case result.kind
            of ndBinOp:
              # Compare operator precednce
              let
                cPres = opToPrecedence[tok.opKind]
                pPres = opToPrecedence[result.origin.opKind]
                d = cmp(cPres, pPres+result.precedence)

              if d <= 0:
                # current op comes after previously parsed
                # so it becomes new root of a tree
                result = Node(kind: ndBinOp, origin: tok, left: result)
              else:
                # current op comes first in order of operations
                # so it "steals" previous right operand as it's left one
                # and assigns itself as right operand of previous operation
                result.right = Node(kind: ndBinOp, origin: tok, left: result.right)

            of ndUnOp:
              # binary op is alwasys after unary op
              # so it becomes new root of the tree
              result = Node(kind: ndBinOp, origin: tok, left: result)
            
            else: unreachable()
        else: unreachable()

    of tkBracket:
      case tok.brKind
      of brLParen:
        # Entering (...) we push outer expr to stack
        # and start parsing inner as if nothing was parsed before
        subExprStack.add result
        result = nil
      of brRParen:
        # Exitiing (...) we check that we were even inside (...) in the first place
        # and that expr in (...) is not void
        p.assertRule(subExprStack.len != 0, tok.pos, "')' doesn't match any '('")
        p.assertRule(result != nil, tok.pos, "empty `()`")

        # Add precedence to expr based on how _inner_ it is
        result.precedence = 100 * subExprStack.len()
        
        # Insert inner expr into outer one
        var prev = subExprStack.pop()
        if prev != nil:
          prev.setRightmostNil(result)
          result = prev
      of brLBrack:
        # Parsing [...] started
        p.eatBracket({brLBrack})

        var listNode = Node(kind: ndList, items: @[p.parseExpression()])
        while p.cTok.kind == tkComma:
          p.eatToken({tkComma})
          let exp = p.parseExpression()
          if exp == nil: break
          listNode.items.add exp
        
        p.eatBracket({brRBrack})
        
        if result != nil:
          result.setRightmostNil(listNode)
        else:
          result = listNode
        tok = p.cTok
        continue
      of brRBrack:
        break
        
    of tkExprEnd, tkEof, tkStmtEnd, tkComma:
      doAssert result != nil
      break
    else:
      unreachable()
    tok = p.advance()


proc parseBlock(p: var Parser, tillStmt: static[string] = ""): Node =
  ## Parse block of code untill `tillStmt` statement is ecnountered,
  ## if none given parses till EoF.
  ## 
  ## Generally returns `ndRope`, but
  ## - when `tillStmt == "endif"` returns `ndIfBlock` with empty `conditionNode` field
  result = Node(
    kind: ndRope,
    rope: @[]
  )

  when tillStmt in ["endif", "endwhile"]:
    
    when tillStmt == "endif":
      const knd = ndIfBlock
    elif tillStmt == "endwhile":
      const knd = ndWhile
    else: error("unreacahble")
    
    var ifBlock = Node(kind: knd, truePath: result)
      ## In ifBlock parsing this is the REAL return value.
      ## The `falsePath` might be assigned during parsing
      ## if `{% else %}` or `{% elif ... %}` are encountered.
      ## The `conditionNode` is empty deliberately and has to be set
      ## by outside block.


  when tillStmt != "":
    var endWordMet = false
  else:
    p.advance()  # ew

  while true:

    case p.state:
    of parseStateQuit: break
    of parseStateText:
      let tok = p.eatToken({tkStr})
      result.rope.add Node(
        kind: ndValue,
        origin: tok
      )

    of parseStateExpr:
      p.eatToken({tkExprStart})
      result.rope.add p.parseExpression()
      p.eatToken({tkExprEnd})
      p.state = parseStateText
    
    of parseStateStmt:
      p.eatToken({tkStmtStart})

      let stmtStart = p.eatToken({tkVar})
      case stmtStart.strValue
      of "set":
        let idTok = p.eatToken({tkVar})
        p.eatOperator({opEq})
        let valNode = p.parseExpression()
        p.eatToken({tkStmtEnd})
        let node = Node(kind: ndSetVar, origin: idTok, varValue: valNode)
        result.rope.add node
      
      of "block":
        let idTok = p.eatToken({tkVar})
        p.eatToken({tkStmtEnd})
        p.state = parseStateText
        var blockNode = p.parseBlock("endblock")
        blockNode.origin = idTok
        result.rope.add blockNode
      
      of "if":
        var conditionNode = p.parseExpression()
        p.eatToken({tkStmtEnd})
        p.state = parseStateText
        var
          node = p.parseBlock("endif")
        node.conditionNode = conditionNode
        node.origin = stmtStart
        result.rope.add node

      of "else":
        
        when tillStmt != "endif":
          p.assertRule(false, p.cTok.pos, "else statement outside of if block")
        else:
          p.eatToken({tkStmtEnd})
          p.state = parseStateText
          let elseBlock = p.parseBlock("endif")
          elseBlock.origin = stmtStart
          elseBlock.conditionNode = Node(kind: ndValue, origin: p.lex.pos.initToken(true))
          endWordMet = true
          ifBlock.falsePath = elseBlock
      
      of "elif":
        when tillStmt != "endif":
          p.assertRule(false, p.cTok.pos, "elif statement outside of if block")
        else:
          var conditionNode = p.parseExpression()
          p.eatToken({tkStmtEnd})
          p.state = parseStateText
          let elifBlock = p.parseBlock("endif")
          elifBlock.origin = stmtStart
          elifBlock.conditionNode = conditionNode
          endWordMet = true
          ifBlock.falsePath = elifBlock
      
      of "extends":
        var other = p.parseExpression()
        p.eatToken({tkStmtEnd})
        var node = Node(kind: ndExtends, otherNode: other)
        result.rope.add(node)
      
      of "while":
        var conditionNode = p.parseExpression()
        p.eatToken({tkStmtEnd})
        p.state = parseStateText
        var
          node = p.parseBlock("endwhile")
        node.conditionNode = conditionNode
        node.origin = stmtStart
        result.rope.add node
      
      of tillStmt:
        when tillStmt != "":
          endWordMet = true
          p.eatToken({tkStmtEnd})
          p.state = parseStateText
          break
        else: unreachable()
      else:
        raise OreError.newException:
          fmt"Unknown statement '{stmtStart}' at {stmtStart.pos.humanRepr}"
      p.state = parseStateText
    
    case p.cTok.kind
    of tkExprStart:
      p.state = parseStateExpr
    of tkStmtStart:
      p.state = parseStateStmt
    of tkEof:
      p.state = parseStateQuit
    else: discard
  
  when tillStmt == "": discard
  elif tillStmt in ["endif", "endwhile"]:
    result = ifBlock
  else:
    p.assertRule(
      endWordMet, p.lex.pos,
      "Block needs to be closed with '{% " & tillStmt & " %}' statement, yet block ended abruptly"
    )


type
  VariantKind* = enum
    varNull = "Null"
    varInt = "Int"
    varFloat = "Float"
    varStr = "Str"
    varBool = "Bool"
    varList = "List"

  Variant* = object
    ## This object represents a variable inside OreEngine
    origin*: Node 
      ## AST node that produced this value
      ## either value or expression 
    case kind*: VariantKind
    of varNull: discard
    of varInt:
      intValue*: int
    of varFloat:
      floatValue*: float
    of varStr:
      strValue*: string
    of varBool:
      boolValue*: bool
    of varList:
      items*: seq[Variant]


func null*(s:typedesc[Variant], origin: Node = nil): Variant =
  ## Return Null variant value
  ## 
  ## Origin is an AST Node that produced this value
  Variant(kind: varNull, origin: origin)
func toVariant*[T](v: T, origin: Node = nil): Variant =
  ## Convert given value to variant
  ## 
  ## Origin is an AST Node that produced this value
  when T is Token:
    case v.kind
    of tkStr:
      result = v.strValue.toVariant(origin)
    of tkInt:
      result = v.intValue.toVariant(origin)
    of tkFloat:
      result = v.floatValue.toVariant(origin)
    of tkBool:
      result = v.boolValue.toVariant(origin)
    else:
      raise OreError.newException:
        fmt"Can't convert token {v.kind} to variant at {v.pos.humanRepr}"
  elif T is     int: result = Variant(kind: varInt,   intValue:   v, origin: origin)
  elif T is   float: result = Variant(kind: varFloat, floatValue: v, origin: origin)
  elif T is  string: result = Variant(kind: varStr,   strValue:   v, origin: origin)
  elif T is    bool: result = Variant(kind: varBool,  boolValue:  v, origin: origin)
  elif T is Variant: result = v
  elif T is seq:
    let items = collect:
      for i in v:
        i.toVariant()
    result = Variant(kind: varList, items: items)
  else: {.error: "Unsupported type for conversion to Variant: " & $T.}

template everyKind*(val: Variant, name, body, onNull: untyped): untyped =
  ## Utility template.
  ## Assigns variant value to `name`
  ## and does `body` for each branch.
  ## Except `varNull`, `varNull` does `onNull` branch
  case val.kind
  of varInt:
    let name = val.intValue
    body
  of varFloat:
    let name = val.floatValue
    body
  of varBool:
    let name = val.boolValue
    body
  of varStr:
    let name = val.strValue
    body
  of varList:
    let name = val.items
    body
  of varNull:
    onNull

template everyKind*(val: Variant, name, body: untyped): untyped =
  ## Utility template.
  ## Assigns variant value to `name`
  ## and does `body` for each branch.
  ## Except `varNull`, `varNull` is discarded
  val.everyKind(name):
    body
  do: discard

func humanRepr*(v: Variant): string =
  result = "Variant"
  v.everyKind(x):
    result &= "[" & $v.kind & "]"
    when typeof(x) is seq[Variant]:
      result &= "(@["
      var idx = 0
      for i in x:
        result &= i.humanRepr()
        if idx != x.high:
          result &= ", "
        idx += 1
      result &= "])"
    else:
      result &= "(" & $x & ")"

func `$`*(v: Variant): string =
  v.everyKind(x):
    when x is string: result =  x
    else:             result = $x
  do: # onNull
    result = "null"

template tryReturn(op: untyped) =
  ## If the op compiles, return it
  when compiles(op): return op

func isTruthy*(n: Variant): bool =
  ## Check if variant value is truthy
  ## 
  ## - booleans are returned as-is
  ## - int and float are truthy when not equal to zero
  ## - string is truthy if it is not empty or whitespace
  ## - seq is truthy if it's not empty
  ## - null is never truthy
  template checkTrue(v): untyped =
    when v is   bool: v
    elif v is    int: v != 0
    elif v is  float: v != 0.0
    elif v is string: not v.isEmptyOrWhitespace()
    elif v is    seq: not v.len == 0
    else: {.error: "Unsupported type".}
  n.everyKind(x):
    return checkTrue(x)
  do: # onNull
    return false

func contains*[T](a: seq[Variant], b: T): bool =
  result = false
  let val = b.toVariant()
  for i in a:
    if (i == val).isTruthy():
      result = true
      break

macro genBinOp(opName: untyped) =
  ## Generate binary operation for all possible
  ## Variant types combinations
  let
    opStr = $opName
    a = ident("a")
    b = ident("b")
    op = ident("op")

  result = quote do:
    func `opName`*(`a`, `b`: Variant, `op`: Node = nil): Variant =
      when `opStr` in ["in", "notin"]:
        case `b`.kind
        of varList:
          case `opStr`
          of "in":
            let r = `b`.items.contains(`a`)
            return r.toVariant(`op`)
          of "notin":
            let r = not `b`.items.contains(`a`)
            return r.toVariant(`op`)
        else: discard
      
      `a`.everyKind(x):
        `b`.everyKind(y):
          when typeof(x) is typeof(y):
            tryReturn(`opName`(x,  y).toVariant(`op`))
          elif typeof(x) is string:
            tryReturn(`opName`(x, $y).toVariant(`op`))
          else:
            tryReturn(`opName`(x, typeof(x)(y)).toVariant(`op`))
      
      # if none of the above yielded results raise
      let lnInfo = 
        if `op` != nil:
          " at " & $`op`.getNodePos().humanRepr
        else:
          ""
      raise OreError.newException:
        (
          "Unsupported operation between " &
          $`a` & " (" & $`a`.kind & ") and " &
          $`b` & " (" & $`b`.kind & ") - '" & $`opStr` & "'" &
          lnInfo
        )

macro genUnOp(opName: untyped) =
  ## Generate unary operation for every possible Variant type
  let
    opStr = $opName
    a = ident("a")
    op = ident("op")
  quote do: 
    func `opName`*(`a`: Variant, `op`: Node = nil): Variant =
      `a`.everyKind(x):
        tryReturn(`opName`(x).toVariant(`op`))
      # if none of the above yielded results raise
      let lnInfo = 
        if `op` != nil:
          " at " & $`op`.getNodePos().humanRepr
        else:
          ""
      raise OreError.newException:
        "Unsupported operation for " & $`a` & "(" & $`a`.kind & " - '" & $`opStr` & "'" & lnInfo 

macro batchGenOps() =
  ## Generate all the possible operations for Variant types
  template callGenFunc(name, target): untyped =
    newCall(ident(name), newTree(kind=nnkAccQuoted, target))

  result = newStmtList()
  for i in opPlus..OperatorKind.high:
    if i in {opEq, opNot}: continue
    let opIdent = ident(opKindToStr[i])
    result.add callGenFunc("genBinOp", opIdent)
  for i in [opPlus, opMinus, opNot]:
    let opIdent = ident(opKindToStr[i])
    result.add callGenFunc("genUnOp", opIdent)

batchGenOps()

type
  OreContext* = ref object
    ctx*: OreContext
    path*: string
    blockOverrides*: Table[string, Node]
    variables: Table[string, Variant]

func initOreContext*(ctx: OreContext = nil, path: string = ""): OreContext =
  result = OreContext(
    ctx: ctx,
    path: path,
    variables: initTable[string, Variant]()
  )

proc setVar*(ctx: var OreContext, name: string, val: Variant) =
  ## Set variable in ore context
  doAssert ctx != nil
  ctx.variables[name] = val

proc getVar*(ctx: OreContext, name: string): Variant =
  ## Get variable in ore context, if doesn't exist
  ## returns Variant.null
  if ctx == nil: return Variant.null
  if name in ctx.variables:
    ctx.variables[name]
  else:
    ctx.ctx.getVar(name)

macro defines*(e: var OreContext, body: untyped): untyped =
  ## Assign variables in engine context
  runnableExamples:
    var engine = initOreContext()
    engine.defines:
      a = 2
      b = 6
    echo engine.renderString("{{ a * b }}")  # 12

  result = newStmtList()
  body.expectKind(nnkStmtList)
  for i in body:
    case i.kind
    of nnkAsgn:
      var name = $i[0]
      var value = i[1]
      result.add quote do:
        `e`.setVar(`name`, `value`.toVariant())
    of nnkCommentStmt: discard
    else: "Not an assignment.".error(i)

proc getBlockOverride(ctx: OreContext, name: string, default: Node = nil): Node =
  if ctx == nil: return default
  if name in ctx.blockOverrides:
    result = ctx.blockOverrides[name]
  else:
    result = ctx.ctx.getBlockOverride(name, default)

proc applyBlockOverride(ctx: var OreContext, name: string) =
  if ctx == nil: return
  if name in ctx.blockOverrides:
    ctx.blockOverrides[name] = nil
  else:
    ctx.ctx.applyBlockOverride(name)

func evalExpression(ctx: OreContext, node: Node): Variant =

  macro genBinOperatorsImpl() =
    ## Generate implementation of every operator for Variant
    result = newStmtList(
      newTree(
        kind=nnkCaseStmt,
        newDotExpr(newDotExpr(ident("node"),ident("origin")),ident("opKind"))
      )
    )

    template getSide(side: string): untyped =
      newCall(
        newDotExpr(ident("ctx"),ident("evalExpression")),
        newDotExpr(ident("node"),ident(side))
      )

    let caseStmt = result[0]
    for i in opPlus..OperatorKind.high:
      if i in {opEq, opNot}: continue  # not applicable, skip
      let
        opIdent = ident(opKindToStr[i])
        impl = newStmtList(
          newAssignment(
            ident("result"),
            newTree(kind=nnkInfix, opIdent, getSide("left"), getSide("right"), ident("node"))
          )
        )
      caseStmt.add(newTree(kind=nnkOfBranch,ident($i),impl))
    caseStmt.add(
      newTree(
        kind=nnkElse,
        newStmtList(
          newTree(
            kind=nnkRaiseStmt,
            newCall(
              newDotExpr(ident("OreError"),ident("newException")),
              newStrLitNode("Operation unsupported.")
            )
          )
        )
      )
    )

  case node.kind
  of ndValue:
    let value = node.origin
    case value.kind
    of tkVar:
      if value.strValue == "null":
        result = Variant.null(node)
      else:
        result = ctx.getVar(value.strValue)
    else:
      result = node.origin.toVariant(node)

  of ndUnOp:
    case node.origin.opKind
    of opMinus:
      result = `-`(ctx.evalExpression(node.operand), node)
    of opPlus:
      result = `+`(ctx.evalExpression(node.operand), node)
    of opNot:
      result = `not`(ctx.evalExpression(node.operand), node)
    else:
      raise OreError.newException:
        fmt"Unsupported unary operator - {node.origin.opKind}"

  of ndBinOp:
    genBinOperatorsImpl()

  of ndList:
    result = Variant(origin: node, kind: varList, items: @[])
    for i in node.items:
      result.items.add ctx.evalExpression(i)
  else: unreachable()

proc parseString(ctx: var OreContext, input: string): Node =
  ## Parse given string.
  ## Saturate context with data nessesary for string construction
  var p = initParser(input)
  result = p.parseBlock()
  doAssert result.kind == ndRope
  for i in result.rope:
    case i.kind
    of ndRope:
      # prepare named block overrides
      if i.origin.kind == tkVar:
        let blockName = i.origin.strValue
        if ctx.getBlockOverride(blockName) == nil:
          ctx.blockOverrides[blockName] = i
    else: discard

proc renderNode(ctx: var OreContext, node: Node): string =
  ## Evaluate given node to string 
  if node == nil: return ""
  case node.kind
  of ndNoOp: discard
  
  of ndRope:
    for i in node.rope.low..node.rope.high:
      let el = node.rope[i]
      case el.kind
      
      of ndExtends:
        let otherVar = ctx.evalExpression(el.otherNode)
        doAssert otherVar.kind == varStr
        let filepath = ctx.path /../ $otherVar
        var subCtx = ctx.initOreContext(filepath)
        node.rope[i] = subCtx.parseString(filepath.readFile())
      
      of ndRope:
        if el.origin.kind == tkVar:
          let blockName = el.origin.strValue
          node.rope[i] = ctx.getBlockOverride(blockName, el)
          ctx.applyBlockOverride(blockName)
      
      else: discard
      result &= ctx.renderNode(node.rope[i])

  of ndValue, ndUnOp, ndBinOp, ndList:
    result &= $ctx.evalExpression(node)

  of ndSetVar:
    doAssert node.origin.kind == tkVar
    ctx.setVar(
      node.origin.strValue,
      ctx.evalExpression(node.varValue)
    )
  
  of ndIfBlock:
    let condition = ctx.evalExpression(node.conditionNode)
    if condition.isTruthy():
      result &= ctx.renderNode(node.truePath)
    else:
      result &= ctx.renderNode(node.falsePath)
  
  of ndWhile:
    let condition = node.conditionNode
    var hasItered = false
    while ctx.evalExpression(condition).isTruthy():
      result &= ctx.renderNode(node.truePath)
      hasItered = true
    if not hasItered:
      result &= ctx.renderNode(node.falsePath)
  
  # all extends nodes should be eliminated at this point in rope branch
  of ndExtends: unreachable()

proc renderString*(ctx: var OreContext, input: string): string =
  var parsed = ctx.parseString(input)
  result = ctx.renderNode(parsed)

proc renderFile*(ctx: var OreContext, filepath: string): string =
  let input = readFile(filepath)
  var subCtx = ctx.initOreContext(filepath)
  subCtx.renderString(input)
