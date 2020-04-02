package duh

package object json5 {
  def parse(file: String): JValue = {
    val source = io.Source.fromFile(file)
    new Parser(source.toArray).parse(Visitor())
  }

  import ujson.Arr
  import scala.annotation.{switch, tailrec}
  import java.lang.Character

  private type Token = Int
  private var maxToken: Int = 0
  private var tokenInit: () => Unit = () => Unit
  private lazy val tokenName = new Array[String](maxToken)
  private def registerToken(name: String): Token = {
    val token = maxToken
    val currTokenInit = tokenInit
    val newTokenInit = () => {
      currTokenInit()
      assert(tokenName(token) == null)
      tokenName(token) = name
    }
    tokenInit = newTokenInit
    maxToken += 1
    token
  }

  private val EOF = registerToken("eof")

  private val NAN = registerToken("NaN")
  private val INFINITY = registerToken("Infinity")
  private val NUMBER = registerToken("number literal")

  private val IDENTIFIER = registerToken("identifier")

  private val STRING = registerToken("string literal")

  private val LEFT_BRACKET = registerToken("[")
  private val LEFT_BRACE = registerToken("{")

  private val RIGHT_BRACKET = registerToken("]")
  private val RIGHT_BRACE = registerToken("}")

  private val COLON = registerToken(":")
  private val COMMA = registerToken(",")
  private val EMPTY = registerToken("empty")
  tokenInit()

  private type ParserState = Int
  private var maxParserState: Int = 0
  private var parserStateInit: () => Unit = () => Unit
  private lazy val parserStateName = new Array[String](maxParserState)
  private lazy val parserStateDebugString = new Array[String](maxParserState)
  private def registerParserState(name: String, debugString: String): Token = {
    val parserState = maxParserState
    val currParserStateInit = parserStateInit
    parserStateInit = () => {
      currParserStateInit()
      assert(parserStateDebugString(parserState) == null)
      parserStateDebugString(parserState) = debugString
      parserStateName(parserState) = name
    }
    maxParserState += 1
    parserState
  }

  private val TOP_VALUE = registerParserState("TOP_VALUE", "expected value")

  private val ARRAY_ELEMENT = registerParserState("ARRAY_ELEMENT", s"expected value or ${tokenName(RIGHT_BRACKET)}")
  private val ARRAY_COMMA = registerParserState("ARRAY_COMMA", s"expected ${tokenName(COMMA)}")

  private val OBJECT_KEY = registerParserState("OBJECT_KEY", s"expected ${tokenName(IDENTIFIER)} or ${tokenName(RIGHT_BRACE)}")
  private val OBJECT_COLON = registerParserState("OBJECT_COLON", s"expected ${tokenName(COLON)}")
  private val OBJECT_VALUE = registerParserState("OBJECT_VALUE", s"expected value}")
  private val OBJECT_COMMA = registerParserState("OBJECT_COMMA", s"expected ${tokenName(COMMA)} or ${tokenName(IDENTIFIER)}")
  parserStateInit()

  private val reservedIdentifiers: Set[String] = Set(
    // keywords
    "break", "do", "instanceof", "typeof",
    "case", "else", "new", "var",
    "catch", "finally", "return", "void",
    "continue", "for", "switch", "while",
    "debugger", "function", "this", "with",
    "default", "if", "throw	", "delete",
    "in", "try",

    // future reserved words
    "class", "enum", "extends", "super",
    "const", "export", "import",
  )

  private val literalIdentifiers: Set[String] = Set(
    "false", "true", "NaN", "Infinity", "null",
  )

  class Parser(source: Array[Char]) {
    private val lexer = new Lexer(source)

    def parse[Context, T](visitor: Visitor[Context, T]): T = {
      val value = rparse(TOP_VALUE, visitor, List.empty)
      lexer.lex()
      if (lexer.token != EOF) {
        error(s"unexpected ${tokenName(lexer.token)}, expected ${tokenName(EOF)}")
      }
      value
    }

    protected def error(msg: String): Nothing =
      throw new Exception(s"at line ${lexer.line}, column ${lexer.column}: $msg")

    protected def error(token: Token, state: ParserState): Nothing =
      error(s"unexpected token ${tokenName(token)}, ${parserStateDebugString(state)}, state: ${parserStateName(state)}")

    @tailrec
    private def rparse[Context, T](
      state: ParserState,
      visitor: BaseVisitor[Context, T],
      stack: List[BaseVisitor[Context, T]]): T = {
      type ArrVisitor = ArrayVisitor[Context, T]
      type ObjVisitor = ObjectVisitor[Context, T]
      type ValVisitor = Visitor[Context, T]

      // just to make things look pretty
      import scala.language.implicitConversions
      @inline implicit def castToObjVisitor(v: BaseVisitor[Context, T]) = v.asInstanceOf[ObjVisitor]
      @inline implicit def castToArrVisitor(v: BaseVisitor[Context, T]) = v.asInstanceOf[ArrVisitor]
      @inline implicit def castToValVisitor(v: BaseVisitor[Context, T]) = v.asInstanceOf[ValVisitor]

      lexer.lex()
      println(tokenName(lexer.token))
      val token = lexer.token
      val context: Context = visitor.getContext(lexer)
      (token: @switch) match {
        case EOF => error(EOF, state)
        case IDENTIFIER =>
          if (literalIdentifiers(lexer.strVal)) {
            val value = lexer.strVal match {
              case "false" => (visitor: ValVisitor).visitFalse(context)
              case "true" => (visitor: ValVisitor).visitTrue(context)
              case "NaN" => (visitor: ValVisitor).visitNaN(context, false)
              case "Infinity" => (visitor: ValVisitor).visitInfinity(context, false)
              case "null" => (visitor: ValVisitor).visitNull(context)
            }
            (state: @switch) match {
              case TOP_VALUE =>
                if (stack.nonEmpty) { error("impossible") }
                value
              case ARRAY_ELEMENT =>
                if (stack.isEmpty) { error("impossible") }
                (stack.head: ArrVisitor).visitElement(value)
                rparse(ARRAY_COMMA, stack.head, stack.tail)
              case OBJECT_VALUE =>
                if (stack.isEmpty) { error("impossible") }
                (stack.head: ObjVisitor).visitValue(value)
                rparse(OBJECT_COMMA, stack.head, stack.tail)
              case _ =>
                error(token, state)
            }
          } else {
            (state: @switch) match {
              case OBJECT_KEY =>
                if (reservedIdentifiers(lexer.strVal)) {
                  error(s"cannot use reserved identifier '${lexer.strVal}' as unquoted object key")
                } else {
                  (visitor: ObjVisitor).visitKey(context, lexer.strVal)
                  rparse(OBJECT_COLON, visitor, stack)
                }
              case _ =>
                error(token, state)
            }
          }
        case LEFT_BRACKET =>
          (state: @switch) match {
            case TOP_VALUE =>
              val subVisitor = (visitor: ValVisitor).arrayVisitor(context)
              rparse(ARRAY_ELEMENT, subVisitor, stack)
            case OBJECT_VALUE | ARRAY_ELEMENT =>
              val subVisitor = (visitor: ValVisitor).arrayVisitor(context)
              rparse(ARRAY_ELEMENT, subVisitor, visitor +: stack)
            case _ =>
              error(token, state)
          }
        case LEFT_BRACE =>
          (state: @switch) match {
            case TOP_VALUE =>
              val subVisitor = (visitor: ValVisitor).objectVisitor(context)
              rparse(OBJECT_KEY, subVisitor, stack)
            case OBJECT_VALUE | ARRAY_ELEMENT =>
              val subVisitor = (visitor: ValVisitor).objectVisitor(context)
              rparse(OBJECT_KEY, subVisitor, visitor +: stack)
            case _ =>
              error(token, state)
          }
        case RIGHT_BRACKET =>
          (state: @switch) match {
            case ARRAY_COMMA | ARRAY_ELEMENT =>
              if (stack.isEmpty) {
                (visitor: ArrVisitor).end(context)
              } else {
                val value = (visitor: ArrVisitor).end(context)
                val nextState = stack.head match {
                  case visitor: ArrVisitor =>
                    visitor.visitElement(value)
                    ARRAY_COMMA
                  case visitor: ObjVisitor =>
                    visitor.visitValue(value)
                    OBJECT_COMMA
                }
                rparse(nextState, stack.head, stack.tail)
              }
            case _ =>
              error(token, state)
          }
        case RIGHT_BRACE =>
          (state: @switch) match {
            case OBJECT_COMMA | OBJECT_KEY =>
              if (stack.isEmpty) {
                (visitor: ObjVisitor).end(context)
              } else {
                val value = (visitor: ObjVisitor).end(context)
                val nextState = stack.head match {
                  case visitor: ArrVisitor =>
                    visitor.visitElement(value)
                    ARRAY_COMMA
                  case visitor: ObjVisitor =>
                    visitor.visitValue(value)
                    OBJECT_COMMA
                }
                rparse(nextState, stack.head, stack.tail)
              }
            case _ =>
              error(token, state)
          }

        case COMMA =>
          (state: @switch) match {
            case OBJECT_COMMA => rparse(OBJECT_KEY, visitor, stack)
            case ARRAY_COMMA => rparse(ARRAY_ELEMENT, visitor, stack)
            case _ => error(token, state)
          }
        case COLON => 
          if (state == OBJECT_COLON) {
            rparse(OBJECT_VALUE, visitor.valueVisitor(context), visitor +: stack)
          } else {
            error(token, state)
          }
        case _ =>
          if (state == OBJECT_KEY) {
            (token: @switch) match {
              case STRING =>
                (visitor: ObjVisitor).visitKey(context, lexer.strVal)
                rparse(OBJECT_COLON, visitor, stack)
              case _ =>
                error(token, state)
            }
          } else {
            val value = (token: @switch) match {
              case NAN => (visitor: ValVisitor).visitNaN(context, lexer.strVal.head == '-')
              case INFINITY => (visitor: ValVisitor).visitInfinity(context, lexer.strVal.head == '-')
              case NUMBER =>
                if (lexer.numberDotIndex() >= 0) {
                  (visitor: ValVisitor).visitInteger(context, lexer.numberBase, lexer.strVal)
                } else {
                  (visitor: ValVisitor).visitFloat(context, lexer.numberDotIndex(), lexer.strVal)
                }
              case STRING => (visitor: ValVisitor).visitString(context, lexer.strVal)
            }

            (state: @switch) match {
              case TOP_VALUE =>
                if (stack.nonEmpty) { error("impossible") }
                value
              case OBJECT_VALUE | ARRAY_ELEMENT =>
                if (stack.isEmpty) { error("impossible") }
                val nextState = stack.head match {
                  case _: ArrayVisitor[_, _] => ARRAY_COMMA
                  case _: ObjectVisitor[_, _] => OBJECT_COMMA
                }
                rparse(nextState, stack.head, stack.tail)
              case _ =>
                error(token, state)
            }
          }
      }
    }
  }

  private val SU = '\u001A' // substitution character

  private type Offset = Int
  class Lexer(source: Array[Char]) {
    private val buffer = new StringBuilder

    // position of current character
    var position: Offset = -1

    // start offset of the current token
    var startOffset: Offset = -1

    // current character
    private var ch: Char = SU
    nextChar()
    

    // the line number of the current position
    var line: Offset = 1

    // the column of the current position
    var column: Offset = 0

    // the type of the current token
    var token: Token = EMPTY

    // the raw string value of the current token
    var strVal: String = null

    // the base of the current token if the current token is a number
    var numberBase: Int = 0

    var numberDotPosition: Int = -1
    def numberDotIndex(): Int = numberDotPosition - startOffset

    var numberExponentPosition: Int = -1
    def numberExponentIndex(): Int = numberExponentPosition - startOffset

    private def setStrVal(): Unit = {
      strVal = buffer.toString
      buffer.clear()
    }



    private def isAtEnd: Boolean = position >= source.length

    private def nextChar(): Unit = {
      position = position + 1
      if (position >= source.length) {
        ch = SU
      } else {
        ch = source(position)
        if (ch == '\n') {
          line += 1
          column = 0
        } else {
          column += 1
        }
      }
    }

    @inline
    private def putChar(c: Char): Unit = {
      println(s"put: $c")
      buffer.append(c)
    }

    private var spanStart = null
    private var spanEnd = null

    private def peek(): Char = {
      if (position < source.length) {
        source(position)
      } else {
        SU
      }
    }

    private def isHexDigit(ch: Char): Boolean = {
      (ch: @switch) match {
        case '0' | '1' | '2' | '3' | '4' |
             '5' | '6' | '7' | '8' | '9' |
             'a' | 'b' | 'c' | 'd' | 'e' | 'f' |
             'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          true
        case _ => false
      }
    }

    private def hexDigit(ch: Char): Int = {
      if (ch <= '9') {
        ch - '0'
      } else if ('a' <= ch && ch <= 'z') {
        ch - 'a' + 10
      } else if ('A' <= ch && ch <= 'Z') {
        ch - 'A' + 10
      } else {
        -1
      }
    }

    protected def error(msg: String): Nothing = throw new Exception(msg)

    private def unicodeEscape(): Char = {
      val codePoint: Int = 0
      for (i <- 0 until 4) {
        val digit = hexDigit(ch)
        if (digit < 0) {
          error(s"invalid hex digit '$ch' in unicode escape sequence")
        }
        codePoint << 4 | digit
        nextChar()
      }
      codePoint.toChar
    }

    private def literal(str: String): Unit = {
      for (ch <- str) {
        if (ch != peek()) {
          error(s"invalid character, expected '$str'")
        }
        putChar(ch)
        nextChar()
      }
    }

    @inline
    private def isDigit(ch: Char): Boolean = {
      '0' <= ch && ch <= '9'
    }

    private def lexNumber(): Unit = {
      // json5 allows leading +/-
      (ch: @switch) match {
        case '-' | '+' =>
          putChar(ch)
          nextChar()
      }
      numberBase = 10
      numberDotPosition = -1
      numberExponentPosition = -1

      // numeric literal
      (ch: @switch) match {
        case '0' =>
          putChar(ch)
          nextChar()
          (ch: @switch) match {
            case 'x' | 'X' =>
              numberBase = 16
              putChar(ch)
              nextChar()
              while (isHexDigit(ch)) {
                putChar(ch)
                nextChar()
              }
            case '.' =>
              putChar(ch)
              nextChar()
              numberDotPosition = position
              lexFraction()
              if (isDigit(ch)) {
                putChar(ch)
                nextChar()
              } else {
                error("expected digit")
              }
          }
        case '.' =>
          putChar(ch)
          nextChar()
          numberDotPosition = position
          if (isDigit(ch)) {
            putChar(ch)
            nextChar()
          } else {
            error("expected digit")
          }
          lexFraction()
        case 'I' =>
          literal("Infinity")
          setStrVal()
          token = INFINITY
        case 'N' =>
          literal("NaN")
          setStrVal()
          token = NAN
        case _ =>
          if (isDigit(ch)) {
            lexFraction()
          } else {
            error("expected number")
          }
      }
    }

    private def lexFraction(): Unit = {
      // read fraction digits
      while (isDigit(ch)) {
        putChar(ch)
        nextChar()
      }

      (ch: @switch) match {
        case 'e' | 'E' =>
          putChar(ch)
          nextChar()
          numberExponentPosition = position
          if (ch == '-' || ch == '+') {
            putChar(ch)
            nextChar()
          }
          if (isDigit(ch)) {
            putChar(ch)
            nextChar()
          } else {
            error("expected digit")
          }
          while (isDigit(ch)) {
            putChar(ch)
            nextChar()
          }
          setStrVal()
          token = NUMBER
        case other =>
          setStrVal()
          token = NUMBER
      }
    }

    private def lexString(): Unit = {
      (ch: @switch) match {
        case '"' | '\''=>
        case _ => error("expected string")
      }
      val quote = ch
      nextChar()
      lexStringAfterQuote(quote)
    }

    private def lexStringAfterQuote(quote: Char): Unit = {
      while (ch != quote) {
        (ch: @switch) match {
          case '\\' =>
            nextChar()
            (ch: @switch) match {
              // single character escape
              case '\'' => putChar('\''); nextChar()
              case '"' => putChar('"'); nextChar()
              case '\\' => putChar('\\'); nextChar()
              case 'b' => putChar('\b'); nextChar()
              case 'f' => putChar('\f'); nextChar()
              case 'n' => putChar('\n'); nextChar()
              case 'r' => putChar('\r'); nextChar()
              case 't' => putChar('\t'); nextChar()
              case 'v' => putChar('\u000B'); nextChar() // vertical tab

              // \ line continuation
              case '\n'
                | '\u2028' // line separator
                | '\u2029' => // paragraph separater
                nextChar()
              case '\r' =>
                nextChar()
                if (peek() == '\n') {
                  nextChar()
                }

              case '0' =>
                val nextCh = peek()
                if ('0' <= nextCh && nextCh <= '9') {
                  error("invalid escape sequence, number other than '0' may not be escaped")
                }
                putChar('\u0000')
                nextChar()
              case 'x' =>
                nextChar()
                val ch1: Int = ch
                if (!isHexDigit(ch)) {
                  error("invalid hex escape sequence")
                }
                nextChar()
                val ch2: Int = ch
                if (!isHexDigit(ch)) {
                  error("invalid hex escape sequence")
                }
                putChar((ch1 << 4 | ch2).toChar) // TODO: is this right??
                nextChar()
              case 'u' =>
                putChar(unicodeEscape())
                nextChar()

              // 1-9 cannot be escaped
              case '1' | '2' | '3' | '4' | '5' |
                   '6' | '7' | '8' | '9' =>
                error(s"invalid escape character: $ch")

              case SU if isAtEnd =>
                error("unclosed string")

              // any other character can be escaped
              case _ =>
                putChar(ch)
                nextChar()
            }
          case '\n' | '\r' => // paragraph separater
            error("invalid string character, line continuations must be escaped")
          case SU if isAtEnd =>
            error("unclosed string")

          // everything else is valid string
          case other =>
            putChar(ch)
            nextChar()
        }
      }
      // found closing " or '
      nextChar()
      setStrVal()
      token = STRING
    }

    private def skipComment(): Unit = {
      if (ch != '/') {
        error("expected comment")
      }
      nextChar()

      (ch: @switch) match {
        case '/' =>
          nextChar()
          skipSingleLineComment()
        case '*' =>
          nextChar()
          skipMultiLineComment()
        case _ =>
          error(s"expected '/' or '*'")
      }
    }

    @tailrec
    private def skipSingleLineComment(): Unit = {
      (ch: @switch) match {
        case '\n' |
             '\r' |
             '\u2028' | // line separator
             '\u2029' => // pragraph separator
          nextChar()
        case SU if isAtEnd =>
        case other =>
          nextChar()
          skipSingleLineComment()
      }
    }

    @tailrec
    private def skipMultiLineComment(): Unit = {
      (ch: @switch) match {
        case '*' =>
          while (ch == '*') {
            nextChar()
          }
          if (ch != '/') {
            nextChar()
            skipMultiLineComment()
          }
        case SU if isAtEnd =>
          error(s"unclosed mulit-line comment")
        case other =>
          nextChar()
          skipMultiLineComment()
      }
    }

    private def lexIdentfier(): Unit = {
      (ch: @switch) match {
        case 'A' | 'B' | 'C' | 'D' | 'E' |
             'F' | 'G' | 'H' | 'I' | 'J' |
             'K' | 'L' | 'M' | 'N' | 'O' |
             'P' | 'Q' | 'R' | 'S' | 'T' |
             'U' | 'V' | 'W' | 'X' | 'Y' |
             'Z' |
             'a' | 'b' | 'c' | 'd' | 'e' |
             'f' | 'g' | 'h' | 'i' | 'j' |
             'k' | 'l' | 'm' | 'n' | 'o' |
             'p' | 'q' | 'r' | 's' | 't' |
             'u' | 'v' | 'w' | 'x' | 'y' |
             'z' |
             '$' | '_' =>
          putChar(ch)
          nextChar()
          lexIdentfierRest()
        case '\\' =>
          nextChar()
          if (ch != 'u') {
            error("only unicode escape sequences are allowed in identifiers")
          }
          nextChar()
          val uCh = unicodeEscape()
          if (Character.isUnicodeIdentifierStart(uCh) || uCh == '$') {
            putChar(uCh)
            nextChar()
            lexIdentfierRest()
          } else {
            error(s"not a valid start of identifier: $ch")
          }

        case SU => // strangely enough, Character.isUnicodeIdentifierPart(SU) returns true!
            error(s"not a valid start of identifier: $ch")
        case _ =>
          if (Character.isUnicodeIdentifierStart(ch)) {
            putChar(ch)
            nextChar()
            lexIdentfierRest()
          } else {
            error(s"not a valid start of identifier: $ch")
          }
      }
    }

    @tailrec
    private def lexIdentfierRest(): Unit = {
      (ch: @switch) match {
        case 'A' | 'B' | 'C' | 'D' | 'E' |
             'F' | 'G' | 'H' | 'I' | 'J' |
             'K' | 'L' | 'M' | 'N' | 'O' |
             'P' | 'Q' | 'R' | 'S' | 'T' |
             'U' | 'V' | 'W' | 'X' | 'Y' |
             'Z' |
             'a' | 'b' | 'c' | 'd' | 'e' |
             'f' | 'g' | 'h' | 'i' | 'j' |
             'k' | 'l' | 'm' | 'n' | 'o' |
             'p' | 'q' | 'r' | 's' | 't' |
             'u' | 'v' | 'w' | 'x' | 'y' |
             'z' |
             '0' | '1' | '2' | '3' | '4' |
             '5' | '6' | '7' | '8' | '9' |
             '$' | '_' =>
          putChar(ch)
          nextChar()
          lexIdentfierRest()
        case '\\' =>
          nextChar()
          if (ch != 'u') {
            error("only unicode escape sequences are allowed in identifiers")
          }
          nextChar()
          val uCh = unicodeEscape()
          if (Character.isUnicodeIdentifierPart(uCh)) {
            putChar(uCh)
            nextChar()
            lexIdentfierRest()
          } else {
            //error(s"not a valid part of identifier: $ch")
            setStrVal()
            token = IDENTIFIER
          }

        case SU =>
          if (!isAtEnd) {
            //error(s"not a valid part of identifier: $ch")
            setStrVal()
            token = IDENTIFIER
          }
        case _ =>
          if (Character.isUnicodeIdentifierPart(ch)) {
            putChar(ch)
            nextChar()
            lexIdentfierRest()
          } else {
            //error(s"not a valid part of identifier: $ch")
            setStrVal()
            token = IDENTIFIER
          }
      }
    }

    @tailrec
    final def lex(): Unit = {
      println(s"LEX $ch")
      startOffset = position
      (ch: @switch) match {
        case '\t'
           | '\u000B' // vertical tab
           | '\f'
           | ' '
           | '\u00A0' // no-break space
           | '\uFEFF' // byte order mark
           | '\n'
           | '\r'
           | '\u2028' // line separator
           | '\u2029' => // paragraph separater
            nextChar()
            lex()
        case '/' =>
          skipComment()
          lex()
        case 'A' | 'B' | 'C' | 'D' | 'E' |
             'F' | 'G' | 'H' | 'I' | 'J' |
             'K' | 'L' | 'M' | 'N' | 'O' |
             'P' | 'Q' | 'R' | 'S' | 'T' |
             'U' | 'V' | 'W' | 'X' | 'Y' |
             'Z' |
             'a' | 'b' | 'c' | 'd' | 'e' |
             'f' | 'g' | 'h' | 'i' | 'j' |
             'k' | 'l' | 'm' | 'n' | 'o' |
             'p' | 'q' | 'r' | 's' | 't' |
             'u' | 'v' | 'w' | 'x' | 'y' |
             'z' |
             '$' | '_' | '\\' =>
          lexIdentfier()
        case '{' | '}' | '[' | ']' | ':' | ',' =>
          (ch: @switch) match {
            case '{' => token = LEFT_BRACE
            case '}' => token = RIGHT_BRACE
            case '[' => token = LEFT_BRACKET
            case ']' => token = RIGHT_BRACKET
            case ':' => token = COLON
            case ',' => token = COMMA
          }
          putChar(ch)
          nextChar()
          setStrVal()
        case '0' | '1' | '2' | '3' | '4' |
             '5' | '6' | '7' | '8' | '9' |
             '+' | '-' | '.' =>
          lexNumber()
        case '"' | '\'' =>
          lexString()
        case _ =>
          if (Character.isUnicodeIdentifierStart(ch)) {
            lexIdentfier()
          } else if (ch == SU && isAtEnd) {
            token = EOF
          } else {
            error("illegal character '\\u%04x'".format(ch: Int))
          }
      }
    }
  }
}
