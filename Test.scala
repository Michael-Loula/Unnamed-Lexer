object Kind extends Enumeration {
  val Undefined, LeftParen, RightParen, Alpha, Num, Dot, Plus, Minus, Asterisk, Slash = Value
}

class Token(kind : Kind.Value, lexeme : String) {
  def this(kind : Kind.Value) { this(kind, "") }
  def getKind: Kind.Value = kind
  def getLexeme: String = lexeme
  def ==(kind : Kind.Value): Boolean = kind match {
    case this.kind => true
    case _ => false
  }
  def !=(kind : Kind.Value): Boolean = ( !(this.kind == kind) )
  def in(kind: Kind.Value*): Boolean = {
    for (k <- kind) if (k == this.kind) return true
    false
  }

}

class Lexer(s: String) {
  var t = s.toList
  def DetectSpace(c: Char): Boolean = c match {
    case ' ' => true
    case '\n' => true
    case _ => false
  }
  def DetectDigit(c: Char): Boolean = c match {
    case '1' => true
    case '2' => true
    case '3' => true
    case '4' => true
    case '5' => true
    case '6' => true
    case '7' => true
    case '8' => true
    case '9' => true
    case '0' => true
    case '.' => true
    case  _  => false
  }
  def DetectAlpha(c: Char): Boolean = c match {
    case 'a' => true
    case 'b' => true
    case 'c' => true
    case 'd' => true
    case 'e' => true
    case 'f' => true
    case 'g' => true
    case 'h' => true
    case 'i' => true
    case 'j' => true
    case 'k' => true
    case 'l' => true
    case 'm' => true
    case 'n' => true
    case 'o' => true
    case 'p' => true
    case 'q' => true
    case 'r' => true
    case 's' => true
    case 't' => true  
    case 'u' => true
    case 'v' => true
    case 'w' => true
    case 'x' => true
    case 'y' => true
    case 'z' => true
    case _ => false
  }
  def peek: Char = t.head
  def get(): Char = {
    val h = t.head
    t = t.tail
    h
  }
  def Identifier(initial: Char): Token = {
    var l = List(initial)
    while (t.length >= 1 && DetectAlpha(peek)) l = get() :: l
    new Token(Kind.Alpha,l.reverse.mkString(""))
  }
  def Numeral(initial: Char): Token = {
   var l = List(initial)
   while (t.length >= 1 && DetectDigit(peek)) l = get() :: l
   new Token(Kind.Num,l.reverse.mkString(""))
  }

  def next(): Token = {
    while (DetectSpace(peek)) t = t.tail
    val initial = get()
    if (DetectAlpha(initial)) return Identifier(initial)
    if (DetectDigit(initial)) return Numeral(initial)
    initial match {
      case '+' => new Token(Kind.Plus,"+")
      case '-' => new Token(Kind.Minus,"-")
      case '*' => new Token(Kind.Asterisk,"*")
      case '/' => new Token(Kind.Slash,"/")
      case '(' => new Token(Kind.LeftParen,"(")
      case ')' => new Token(Kind.RightParen,")")
      case _   => new Token(Kind.Undefined)
    }
  }

}


object Test {
  def main(arg: Array[String]) {
    val S = "xxyy * xyx"
    val lex = new Lexer(S)
    for ( i <- 1 to 3) println(lex.next().getLexeme)
  }
}
