import strformat
import strutils
import math
import tables
import macros

type
  CodePos* = tuple[offset, line, col: int]

  TokenKind* = enum
    tkEof
    
    tkOperator
    
    tkStr, tkInt, tkFloat,
    tkBool, tkVar

    tkExprStart, tkExprEnd  ## {{ and }}
    tkBracket
  
  OperatorKind* = enum
    opDot,
    opPlus, opMinus
    opMult, opDivide

  BracketKind* = enum
    brLParen, brRParen,
    brLBrack, brRBrack

  Token* = object
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
const opKindToStr*: array[OperatorKind, string] = [".", "+", "-", "*", "/"]
const opToPrecendece*: array[OperatorKind, int] = [100, 1, 1, 2, 2]
const brKindToStr*: array[BracketKind, string] = ["(", ")", "[", "]"]



func initToken*(pos: CodePos, kind: static[TokenKind]): Token =
  ## Create valueless token, only accepts valueless token kinds
  when kind notin {tkEof, tkExprStart, tkExprEnd}:
    {.error: "Only valueless token kinds are acceptable in this constructor".}
  Token(
    pos: pos,
    kind: kind
  )

func initToken*(pos: CodePos, brKind: BracketKind): Token =
  ## Create bracket token
  Token(
    pos: pos,
    kind: tkBracket,
    brKind: brKind
  )

func initToken*(pos: CodePos, opKind: OperatorKind): Token =
  ## Create opearator token
  Token(
    pos: pos,
    kind: tkOperator,
    opKind: opKind
  )

func initToken*(pos: CodePos, value: string, quoted: bool, kind: static[TokenKind] = tkStr): Token =
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

func initToken*(pos: CodePos, value: int): Token =
  ## Create token from integer value
  Token(
    pos: pos,
    kind: tkInt,
    intValue: value
  )

func initToken*(pos: CodePos, value: float): Token =
  ## Create token from float value
  Token(
    pos: pos,
    kind: tkFloat,
    floatValue: value
  )

func initToken*(pos: CodePos, value: bool): Token =
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

func humanRepr*(pos: CodePos): string =
  fmt"line {pos.line}, col {pos.col} (offset: {pos.offset})"

func humanRepr*(tk: Token): string =
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
  of tkBracket:
    result &= fmt"""bracket "{brKindToStr[tk.brKind]}" """
  
  result &= fmt"at {tk.pos.humanRepr}"


type
  LexState* = enum
    lexText
    lexExpr

  Lexer* = object
    pos*: CodePos
    state*: LexState
    input*: string
    cChar*: char


func initLexer*(input: string): Lexer =
  ## Create lexer for given text
  Lexer(
    pos: (-1, 1, 0),
    state: lexText,
    input: input,
    cChar: '\0'
  )

func isFinished*(lex: Lexer): bool =
  (lex.input.high == -1) or (lex.pos.offset > lex.input.high)


proc advance*(lex: var Lexer): char {.discardable.} =
  ## Advance lexer by one char
  lex.pos.offset += 1
  
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

func peek*(lex: Lexer, offset: int = 1): char =
  ## Return char that is offset characters away from current
  let pos = lex.pos.offset + offset

  if pos > lex.input.high:
    result = '\0'
  else:
    result = lex.input[pos]


proc skipWhitespace(lex: var Lexer): char =
  while lex.cChar in {' ', '\t', '\n', '\r'}:
    lex.advance
  result = lex.cChar


proc parseNum(lex: var Lexer): Token =
  # Behold, most efficient number parsing
  let pos = lex.pos
  var
    c = lex.cChar
    numInt = (c.byte - '0'.byte).int
    next = lex.peek

  while next.isDigit:
    lex.advance
    c = lex.cChar
    numInt = numInt * 10 + (c.byte - '0'.byte).int
  if next == '.':
    next = lex.peek(2)
    var
      exp = 1.0
      numFloat = 0.0
    
    if next.isDigit:
      lex.advance
      while next.isDigit:
        lex.advance
        c = lex.cChar
        numFloat = numFloat + (c.byte - '0'.byte).float / pow(10'f, exp)
        exp += 1.0
        next = lex.peek

      result = pos.initToken(numInt.float + numFloat)
    else:
      result = pos.initToken(numInt)
  else:
    result = pos.initToken(numInt)


func getNextToken*(lex: var Lexer): Token =
  lex.advance()

  var c = lex.cChar

  if c == '\0':
    return lex.pos.initToken(tkEof)

  case lex.state
  of lexText:
    let pos = lex.pos
    var text = ""
    while true:
      case c
      of '{':  # Entering code block?
        case lex.peek
        of '{':  # expression block perhaps?
          lex.state = lexExpr
          break
        else:  # Nope, no expression here treat as normal char
          text &= c
      of '\0':
        break
      else:
        text &= c
      c = lex.advance()
    result = pos.initToken(text, false)

  of lexExpr:
    c = lex.skipWhitespace
    let pos = lex.pos
    
    case c
    of '{':
      result = pos.initToken(tkExprStart)
      lex.advance()
    
    of '}':
      if lex.peek == '}':
        result = pos.initToken(tkExprEnd)
        lex.advance()
        lex.advance()
        lex.state = lexText
      else:
        raise OreError.newException:
          fmt"Unexpected token: {lex.peek} at {pos.humanRepr}"
    
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

    else:
      let opIdx = opKindToStr.find($c)
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
          result = pos.initToken(ident, false, tkVar)
        return


type
  NodeKind* = enum
    ndNoOp
    ndValue
    ndRope
    ndUnOp
    ndBinOp

  Node* = ref object
    precedence*: int
    case kind*: NodeKind
    of ndValue:
      value*: Token
    of ndRope:
      rope*: seq[Node]
    of ndUnOp:
      unOp*: Token
      operand*: Node
    of ndBinOp:
      binOp*: Token
      left*: Node
      right*: Node
    else: discard

  Parser* = object
    lex*: Lexer
    cTok*: Token


func treeRepr*(n: Node, indent: int = 0): string =
  result = indent.spaces & "|"
  if n == nil:
    result &= "nil\n"
    return
  const deltaIndent = 4

  case n.kind
  of ndValue:
    result &= fmt"Value: <{n.value}> ({n.value.kind})"
  of ndRope:
    result &= "Rope:\n"
    for i in n.rope:
      result &= i.treeRepr(indent + deltaIndent)
  of ndUnOp:
    result &= fmt"Unary operator ({n.unOp}):" & '\n'
    result &= n.operand.treeRepr(indent + deltaIndent)
  of ndBinOp:
    result &= fmt"Binary operator ({n.binOp}):" & '\n'
    result &= n.left.treeRepr(indent + deltaIndent)
    result &= n.right.treeRepr(indent + deltaIndent)
  of ndNoOp:
    result &= "NoOp"
  
  if not result.endsWith('\n'):
    result &= '\n'


func isConst*(n: Node): bool =
  ## Check if node can be evaluated
  ## without any external
  if n == nil: return false

  case n.kind
  of ndValue:
    result = n.value.kind != tkVar
  of ndRope:
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


func initParser*(s: string): Parser =
  Parser(
    lex: initLexer(s)
  )


func lexState*(p: var Parser): LexState {.inline.} = p.lex.state


proc advance*(p: var Parser): Token {.discardable.} =
  p.cTok = p.lex.getNextToken()
  p.cTok


proc eatToken*(p: var Parser, kinds: set[TokenKind]): Token {.discardable.} =
  ## Confirm that current token is one of given kinds, and advances the parser
  ## otherwise, raise OreError.
  ## 
  ## Returns the token in question.
  result = p.cTok
  if p.cTok.kind in kinds:
    discard p.advance()
  else:
    raise OreError.newException:
      fmt"Unexpected token at {result.pos.humanRepr}. Expected {kinds}, got {result.kind}"


proc parseExpression*(p: var Parser): Node =
  # BEHOLD, recursiveless recursive descent expression parsing
  var tok = p.cTok
  var subExprStack: seq[Node]

  template validateSyntax(condition, body: untyped): untyped =
    ## Check condition true: do body, otherwise raise OreError 
    if condition:
      body
    else:
      raise OreError.newException:
        fmt"Invalid syntax at {p.cTok.pos.humanRepr}"

  while tok.kind notin {tkExprEnd, tkEof}:
    case tok.kind
    
    of tkStr, tkBool, tkFloat, tkInt, tkVar:
      let node = Node(kind: ndValue, value: tok)
      if result == nil:
        # Put value node in result if nothing was parsed before
        result = node
      else:
        # if something was parsed before we do...
        case result.kind
        
        of ndUnOp:
          # Put value into Unary operator operand slot
          validateSyntax(result.operand == nil):
            result.operand = node
        
        of ndBinOp:
          # replace the rightmost operand with current value node
          
          var tmp = result  # store original here and descent
          while result.kind == ndBinOp:
            if result.right == nil:
              break
            result = result.right
          validateSyntax(result.kind == ndBinOp):
            result.right = node
          result = tmp   # restore original

        of ndValue:
          # tell that two values in a row is not a valid syntax
          validateSyntax(false): discard
        
        else: discard

    of tkOperator:
      if result == nil:
        result = Node(kind: ndUnOp, unOp: tok)
      else:
        
        # Check previous operation
        case result.kind
        of ndValue:
          result = Node(kind: ndBinOp, binOp: tok, left: result)
        of ndUnOp:
          if result.operand == nil:
            result.operand = Node(kind: ndUnOp, unOp: tok)
          else:
            result = Node(kind: ndBinOp, binOp: tok, left: result)
        of ndBinOp:
          validateSyntax(result.right != nil): discard
          
          # Compare operator precednce
          let
            cPres = opToPrecendece[tok.opKind]
            pPres = opToPrecendece[result.binOp.opKind]
            d = cmp(cPres, pPres+result.precedence)
          
          if d <= 0:
            result = Node(kind: ndBinOp, binOp: tok, left: result)
          else:
            var r = result.right
            result.right = Node(kind: ndBinOp, binOp: tok, left: r)
        else: discard
    of tkBracket:
      case tok.brKind
      of brLParen:
        subExprStack.add result
        result = nil
      of brRParen:
        validateSyntax(subExprStack.len != 0): discard
        validateSyntax(result != nil): discard

        result.precedence = 1000 * subExprStack.len()
        var prev = subExprStack.pop()
        if prev != nil:
          # Check previous operation/
          case prev.kind
          of ndUnOp:
            validateSyntax(prev.operand == nil):
              prev.operand = result
              result = prev
          of ndBinOp:
            validateSyntax(prev.right == nil):
              prev.right = result
              result = prev
          else:
            validateSyntax(false): discard
      else:
        validateSyntax(false): discard
    of tkExprEnd, tkEof: break
    else:
      validateSyntax(false): discard
    tok = p.advance()


proc parseBlock*(p: var Parser): Node =
  result = Node(
    kind: ndRope,
    rope: @[]
  )

  var state = p.lexState()
  p.advance()

  while not p.lex.isFinished():
    case state:
    of lexText:
      let tok = p.eatToken({tkStr, tkExprEnd})
      if tok.kind == tkExprEnd: continue

      result.rope.add Node(
        kind: ndValue,
        value: tok
      )

    of lexExpr:
      p.eatToken({tkExprStart})
      result.rope.add p.parseExpression()
      p.eatToken({tkExprEnd})
    
    state = p.lexState()


type
  VariantKind* = enum
    varNull
    varInt, varFloat, varStr, varBool

  Variant* = object
    ## This object represents a variable inside
    ## OreEngine context
    case kind*: VariantKind
    of varNull: discard
    of varInt:
      intVal*: int
    of varFloat:
      floatVal*: float
    of varStr:
      strVal*: string
    of varBool:
      boolVal*: bool
  
  OreContext* = object
    variables*: Table[string, Variant]
  
  OreFileHandler* = object
    path*: string
    deps*: seq[string]  ## Other files current file depends on
    ctx*: OreContext

  OreEngine* = object
    globalContext*: OreContext


func null*(s:typedesc[Variant]): Variant = Variant(kind: varNull)
converter toVariant*(v: int): Variant = Variant(kind: varInt, intVal: v)
converter toVariant*(v: float): Variant = Variant(kind: varFloat, floatVal: v)
converter toVariant*(v: string): Variant = Variant(kind: varStr, strVal: v)
converter toVariant*(v: bool): Variant = Variant(kind: varBool, boolVal: v)
converter toVariant*(v: Token): Variant =
  template error() =
    raise OreError.newException:
      fmt"Can't convert token {v.kind} to variant at {v.pos.humanRepr}"
  
  case v.kind
  of tkStr:
    result = v.strValue.toVariant()
  of tkInt:
    result = v.intValue.toVariant()
  of tkFloat:
    result = v.floatValue.toVariant()
  of tkBool:
    result = v.boolValue.toVariant()
  else:
    error()


func humanRepr*(v: Variant): string =
  result = "Variant"
  case v.kind
  of varNull:
    result &= "(null)"
  of varInt:
    result &= fmt"(int {v.intVal})"
  of varFloat:
    result &= fmt"(float {v.floatVal})"
  of varStr:
    result &= fmt"(string {v.strVal.escape})"
  of varBool:
    result &= fmt"(bool {v.boolVal})"


func `$`*(v: Variant): string =
  case v.kind
  of varNull:
    result = "null"
  of varInt:
    result = $v.intVal
  of varFloat:
    result = $v.floatVal
  of varStr:
    result = $v.strVal
  of varBool:
    result = $v.boolVal


func initOreEngine*(): OreEngine =
  OreEngine()

proc setVar*(ctx: var OreContext, name: string, val: Variant) =
  ## Set variable in ore context
  ctx.variables[name] = val

proc getVar*(ctx: OreContext, name: string): Variant =
  ## Get variable in ore context, if doesn't exist
  ## returns Variant.null
  ctx.variables.getOrDefault(name, Variant.null)


macro genMath(opName: untyped) =
  let opStr = $opName
  quote do:
    func `opName`*(a, b: Variant): Variant =
      template oreError() =
        raise OreError.newException:
          "Unsupported operation between " & $a & "(" & $a.kind & " and " & $b & "(" & $b.kind & ") - '" & $`opStr` & "'"
      case a.kind

      of varInt:
        let x = a.intVal
        case b.kind:
        of varInt:
          let y = b.intVal
          result = `opName`(x, y).toVariant()
        of varFloat:
          let y = b.floatVal
          result = `opName`(x.float, y).toVariant()
        else: oreError()

      of varFloat:
        let x = a.floatVal
        case b.kind:
        of varInt:
          let y = b.intVal
          result = `opName`(x, y.float).toVariant()
        of varFloat:
          let y = b.floatVal
          result = `opName`(x, y).toVariant()
        else:oreError()
      of varStr:
        if `opStr` == "+":
          result = ($a & $b).toVariant()
        else: oreError()
      else: oreError()


genMath(`+`)
genMath(`-`)
genMath(`*`)
genMath(`/`)



macro genUnOp(opName: untyped) =
  let opStr = $opName
  quote do: 
    func `opName`*(a: Variant): Variant =
      template oreError() =
        raise OreError.newException:
          "Unsupported operation for " & $a & "(" & $a.kind & " - '" & $`opStr` & "'"
      case a.kind
      of varInt: result = `opName`(a.intVal).toVariant()
      of varFloat: result = `opName`(a.floatVal).toVariant()
      else: oreError()


genUnOp(`-`)
genUnOp(`+`)


func evalExpression(ctx: OreContext, node: Node): Variant =
  case node.kind
  of ndValue:
    let value = node.value
    case value.kind
    of tkVar:
      if value.strValue == "null":
        result = Variant.null()
      else:
        result = ctx.getVar(value.strValue)
    else:
      result = node.value.toVariant()

  of ndUnOp:
    case node.unOp.opKind
    of opMinus:
      result = -ctx.evalExpression(node.operand)
    of opPlus:
      result = +ctx.evalExpression(node.operand)
    else:
      raise OreError.newException:
        fmt"Unsupported unary operator - {node.unOp.opKind}"

  of ndBinOp:
    case node.binOp.opKind
    of opPlus:
      result = ctx.evalExpression(node.left) + ctx.evalExpression(node.right)
    of opMinus:
      result = ctx.evalExpression(node.left) - ctx.evalExpression(node.right)
    of opMult:
      result = ctx.evalExpression(node.left) * ctx.evalExpression(node.right)
    of opDivide:
      result = ctx.evalExpression(node.left) / ctx.evalExpression(node.right)
    else:
      raise OreError.newException:
        "Operation unsupported."

  else: discard


proc renderString*(e: var OreEngine, input: string): string =
  var p = initParser(input)
  var parsed = p.parseBlock()
  doAssert parsed.kind == ndRope
  
  for i in parsed.rope:
    case i.kind
    of ndValue, ndUnOp, ndBinOp:
      result &= $e.globalContext.evalExpression(i)
    else:
      discard
