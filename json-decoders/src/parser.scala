package duh

package object json5 {

  def parse(file: String): JValue = {
    val source = io.Source.fromFile(file)
    new Parser(source.toArray).parse(Visitor())
  }

  import scala.annotation.{switch, tailrec}

  private type Token = Int

  private var maxToken: Token = 13
  private val tokenName = new Array[String](maxToken)
  private def registerToken(token: Token, name: String): Unit = {
    assert(tokenName(token) == null)
    tokenName(token) = name
  }

  private final val EOF = 0; registerToken(EOF, "eof")

  private final val NAN = 1; registerToken(NAN, "NaN")
  private final val INFINITY = 2; registerToken(INFINITY, "Infinity")
  private final val NUMBER = 3; registerToken(NUMBER, "number literal")

  private final val IDENTIFIER = 4; registerToken(IDENTIFIER, "identifier")

  private final val STRING = 5; registerToken(STRING, "string literal")

  private final val LEFT_BRACKET = 6; registerToken(LEFT_BRACKET, "[")
  private final val LEFT_BRACE = 7; registerToken(LEFT_BRACE, "{")

  private final val RIGHT_BRACKET = 8; registerToken(RIGHT_BRACKET, "]")
  private final val RIGHT_BRACE = 9; registerToken(RIGHT_BRACE, "}")

  private final val COLON = 10; registerToken(COLON, ":")
  private final val COMMA = 11; registerToken(COMMA, ",")
  private final val EMPTY = 12; registerToken(EMPTY, "empty")


  private type ParserState = Int

  private var maxParserState: Int = 7
  private val parserStateName = new Array[String](maxParserState)
  private val parserStateDebugString = new Array[String](maxParserState)
  private def registerParserState(state: ParserState, name: String, debugString: String): Unit = {
    assert(parserStateDebugString(state) == null)
    parserStateDebugString(state) = debugString
    parserStateName(state) = name
  }

  private final val TOP_VALUE = 0; registerParserState(TOP_VALUE, "TOP_VALUE", "expected value")

  private final val ARRAY_ELEMENT = 1; registerParserState(ARRAY_ELEMENT, "ARRAY_ELEMENT", s"expected value or ${tokenName(RIGHT_BRACKET)}")
  private final val ARRAY_COMMA = 2; registerParserState(ARRAY_COMMA, "ARRAY_COMMA", s"expected ${tokenName(COMMA)}")

  private final val OBJECT_KEY = 3; registerParserState(OBJECT_KEY, "OBJECT_KEY", s"expected ${tokenName(IDENTIFIER)} or ${tokenName(RIGHT_BRACE)}")
  private final val OBJECT_COLON = 4; registerParserState(OBJECT_COLON, "OBJECT_COLON", s"expected ${tokenName(COLON)}")
  private final val OBJECT_VALUE = 5; registerParserState(OBJECT_VALUE, "OBJECT_VALUE", s"expected value")
  private final val OBJECT_COMMA = 6; registerParserState(OBJECT_COMMA, "OBJECT_COMMA", s"expected ${tokenName(COMMA)} or ${tokenName(IDENTIFIER)}")

  // print warning if useing reserved identifier as key?
  private final val reservedIdentifiers: Set[String] = Set(
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

  private final val literalIdentifiers: Set[String] = Set(
    "false", "true", "NaN", "Infinity", "null",
  )

  private class Parser(source: Array[Char]) {
    private val lexer = new Lexer(source)

    def parse[Context, T](visitor: Visitor[Context, T]): T = {
      val value = rparse(TOP_VALUE, visitor, List.empty)
      lexer.lex()
      if (lexer.getToken != EOF) {
        error(s"unexpected ${tokenName(lexer.getToken)}, expected ${tokenName(EOF)}")
      }
      value
    }

    protected def error(msg: String): Nothing =
      throw new Exception(s"at line ${lexer.getLine}, column ${lexer.getColumn}: $msg")

    protected def error(token: Token, state: ParserState): Nothing =
      error(s"unexpected token ${tokenName(token)}, ${parserStateDebugString(state)}, state: ${parserStateName(state)}")

    // just to make things look pretty
    import scala.language.implicitConversions
    @inline implicit def castToObjVisitor[Context, T](v: BaseVisitor[Context, T]) = v.asInstanceOf[ObjectVisitor[Context, T]]
    @inline implicit def castToArrVisitor[Context, T](v: BaseVisitor[Context, T]) = v.asInstanceOf[ArrayVisitor[Context, T]]
    @inline implicit def castToValVisitor[Context, T](v: BaseVisitor[Context, T]) = v.asInstanceOf[Visitor[Context, T]]

    private def visitLiteral[Context, T](
      context: Context,
      valVisitor: Visitor[Context, T],
      token: Token) = {
      (token: @switch) match {
        case NAN => valVisitor.visitNaN(context, lexer.getStrVal.head == '-')
        case INFINITY => valVisitor.visitInfinity(context, lexer.getStrVal.head == '-')
        case NUMBER =>
          if (lexer.getNumberDotIndex >= 0) {
            valVisitor.visitFloat(context, lexer.getNumberDotIndex, lexer.getStrVal)
          } else {
            valVisitor.visitInteger(context, lexer.getNumberBase, lexer.getStrVal)
          }
        case STRING => valVisitor.visitString(context, lexer.getStrVal)
      }
    }

    private def visitIdentifierLiteral[Context, T](
      context: Context,
      valVisitor: Visitor[Context, T],
      identifier: String) = {
      identifier match {
        case "false" => valVisitor.visitFalse(context)
        case "true" => valVisitor.visitTrue(context)
        case "NaN" => valVisitor.visitNaN(context, false)
        case "Infinity" => valVisitor.visitInfinity(context, false)
        case "null" => valVisitor.visitNull(context)
      }
    }

    @tailrec
    private def rparse[Context, T](
      state: ParserState,
      visitor: BaseVisitor[Context, T],
      stack: List[BaseVisitor[Context, T]]): T = {
      type ArrVisitor = ArrayVisitor[Context, T]
      type ObjVisitor = ObjectVisitor[Context, T]
      type ValVisitor = Visitor[Context, T]

      lexer.lex()
      val token = lexer.getToken
      lazy val context: Context = visitor.getContext(lexer)
      (token: @switch) match {
        case EOF => error(EOF, state)
        case IDENTIFIER =>
          (state: @switch) match {
            case OBJECT_KEY =>
              (visitor: ObjVisitor).visitKey(context, lexer.getStrVal)
              rparse(OBJECT_COLON, visitor, stack)
            case TOP_VALUE =>
              if (stack.nonEmpty) { error("impossible") }
              visitIdentifierLiteral(context, visitor: ValVisitor, lexer.getStrVal)
            case ARRAY_ELEMENT =>
              val value = visitIdentifierLiteral(context, (visitor: ArrVisitor).elementVisitor(), lexer.getStrVal)
              (visitor: ArrVisitor).visitElement(value)
              rparse(ARRAY_COMMA, visitor, stack)
            case OBJECT_VALUE =>
              val value = visitIdentifierLiteral(context, (visitor: ObjVisitor).valueVisitor(), lexer.getStrVal)
              (visitor: ObjVisitor).visitValue(value)
              rparse(OBJECT_COMMA, visitor, stack)
            case _ =>
              error(token, state)
          }
        case LEFT_BRACKET =>
          (state: @switch) match {
            case TOP_VALUE =>
              val subVisitor = (visitor: ValVisitor).arrayVisitor(context)
              rparse(ARRAY_ELEMENT, subVisitor, stack)
            case OBJECT_VALUE =>
              val subVisitor = (visitor: ObjVisitor).valueVisitor().arrayVisitor(context)
              rparse(ARRAY_ELEMENT, subVisitor, visitor +: stack)
            case ARRAY_ELEMENT =>
              val subVisitor = (visitor: ArrVisitor).elementVisitor().arrayVisitor(context)
              rparse(ARRAY_ELEMENT, subVisitor, visitor +: stack)
            case _ =>
              error(token, state)
          }
        case LEFT_BRACE =>
          (state: @switch) match {
            case TOP_VALUE =>
              val subVisitor = (visitor: ValVisitor).objectVisitor(context)
              rparse(OBJECT_KEY, subVisitor, stack)
            case OBJECT_VALUE =>
              val subVisitor = (visitor: ObjVisitor).valueVisitor().objectVisitor(context)
              rparse(OBJECT_KEY, subVisitor, visitor +: stack)
            case ARRAY_ELEMENT =>
              val subVisitor = (visitor: ArrVisitor).elementVisitor().objectVisitor(context)
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
            rparse(OBJECT_VALUE, visitor, stack)
          } else {
            error(token, state)
          }
        case _ =>
          (state: @switch) match {
            case OBJECT_KEY =>
              (token: @switch) match {
                case STRING =>
                  (visitor: ObjVisitor).visitKey(context, lexer.getStrVal)
                  rparse(OBJECT_COLON, visitor, stack)
                case _ =>
                  error(token, state)
              }
            case TOP_VALUE =>
              if (stack.nonEmpty) { error("impossible") }
              visitLiteral(context, visitor: ValVisitor, token)
            case OBJECT_VALUE =>
              val value = visitLiteral(context, (visitor: ObjVisitor).valueVisitor(), token)
              (visitor: ObjVisitor).visitValue(value)
              rparse(OBJECT_COMMA, visitor, stack)
            case ARRAY_ELEMENT =>
              val value = visitLiteral(context, (visitor: ObjVisitor).elementVisitor(), token)
              (visitor: ArrVisitor).visitElement(value)
              rparse(ARRAY_COMMA, visitor, stack)
            case _ =>
              error(token, state)
          }
      }
    }
  }

  private final val SU = '\u001A' // substitution character

  private type Offset = Int
  class Lexer(source: Array[Char]) {
    private val buffer = new StringBuilder

    // position of current character
    private var position: Offset = -1
    def getPosition: Offset = position

    // start offset of the current token
    private var startOffset: Offset = -1
    def getStartOffset: Offset = startOffset

    // current character
    private var ch: Char = SU
    nextChar()
    
    // the line number of the current position
    private var line: Offset = 1
    def getLine: Offset = line

    // the column of the current position
    private var column: Offset = 0
    def getColumn: Offset = column

    // the type of the current token
    private var token: Token = EMPTY
    def getToken: Offset = token

    // the raw string value of the current token
    private var strVal: String = null
    def getStrVal: String = strVal

    // the base of the current token if the current token is a number
    private var numberBase: Int = 0
    def getNumberBase: Offset = numberBase

    private var numberDotPosition: Int = -1
    def getNumberDotIndex: Int = numberDotPosition - startOffset

    private var numberExponentPosition: Int = -1
    def getNumberExponentIndex: Int = numberExponentPosition - startOffset

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

    protected def error(msg: String): Nothing = throw new Exception(s"at line $line, column $column:" + msg)

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
        case _ =>
      }
      numberBase = 10
      numberDotPosition = -1
      numberExponentPosition = -1

      // numeric literal
      (ch: @switch) match {
        case '0' =>
          nextChar()
          (ch: @switch) match {
            case 'x' | 'X' =>
              numberBase = 16
              nextChar()
              while (isHexDigit(ch)) {
                putChar(ch)
                nextChar()
              }
              token = NUMBER
              setStrVal()
            case '.' =>
              putChar('0')
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
            case _ =>
              putChar('0')
              setStrVal()
              token = NUMBER
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
          case SU =>
            if (isAtEnd) {
              error("unclosed string")
            } else {
              putChar(ch)
              nextChar()
            }
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
          nextChar()
          if (ch == '/') {
            nextChar()
          } else {
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
            error("invalid escape sequence '\\$ch', only unicode escape sequences are allowed in identifiers")
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
            error("invalid escape sequence '\\$ch', only unicode escape sequences are allowed in identifiers")
          }
          nextChar()
          val uCh = unicodeEscape()
          if (Character.isUnicodeIdentifierPart(uCh) || uCh == '$') {
            putChar(uCh)
            nextChar()
            lexIdentfierRest()
          } else {
            setStrVal()
            token = IDENTIFIER
          }

        case SU => // isUnicodeIdentifierPart returns true for SU?
          setStrVal()
          token = IDENTIFIER
        case _ =>
          if (Character.isUnicodeIdentifierPart(ch)) {
            putChar(ch)
            nextChar()
            lexIdentfierRest()
          } else {
            setStrVal()
            token = IDENTIFIER
          }
      }
    }

    @tailrec
    final def lex(): Unit = {
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
        case '{' =>
          token = LEFT_BRACE
          putChar(ch)
          nextChar()
          setStrVal()
        case '}' =>
        token = RIGHT_BRACE
          putChar(ch)
          nextChar()
          setStrVal()
        case '[' =>
          token = LEFT_BRACKET
          putChar(ch)
          nextChar()
          setStrVal()
        case ']' =>
          token = RIGHT_BRACKET
          putChar(ch)
          nextChar()
          setStrVal()
        case ':' =>
          token = COLON
          putChar(ch)
          nextChar()
          setStrVal()
        case ',' =>
          token = COMMA
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
