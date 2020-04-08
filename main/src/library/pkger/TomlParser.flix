namespace Toml {

    pub enum ParsedToml {
        case MapVar(Map[String, ParsedToml]),
        case StringVal(String),
        case DateVal(String),
        case FloatVal(String),
        case ListVal(List[ParsedToml]),
        case BoolVal(Bool),
        case IntVal(Int)
    }

    pub def parse(input: String): ParsedToml = ???

    def start(
        input: List[Char],
        acc: ParsedToml
      ): (List[Char], ParsedToml) = match List.head(input) {
      case Some(c) => if (isWhiteSpace(c) || isLF(c)) {
        start(List.drop(1, input), acc)
      } else if (isCR(c)) {
        let input0 = stripCRLF(List.drop(1, input));
        start(input0, acc)
      } else if (c == '#') {
        let input0 = stripComment(List.drop(1, input));
        ???
      } else {
        ???
      }
      case None => ???
    }

    def parseKey(
      input: List[Char],
      prefix: List[String],
      acc: ParsedToml
    ): (List[Char], ParsedToml) =

    def parseBareKey(
      input: List[Char],
      keyAcc: List[Char],
      prefix: List[String],
      acc: ParsedToml
    ): (List[Char], ParsedToml) = match List.head(input) {
      case Some(c) => if (isKeyChar(c)) {
        parseBareKey(List.drop(1, input), c :: keyAcc, prefix, acc)
      } else if (isWhiteSpace(c)) {
        parseWhiteSpaceAfterKey(List.drop(1, input), List.toString(keyAcc) :: prefix, acc) 
      } else if (c == '.') {
        parseKey(List.drop(1, input), List.toString(keyAcc) :: prefix, acc)
      } else if (c == '=') {
        parseValue(List.drop(1, input), List.toString(keyAcc) :: prefix, acc)
      } else {
        ???
      }
      case None => ???
    }

    def parseWhiteSpaceAfterKey(
      input: List[Char],
      prefix: List[String],
      acc: ParsedToml
    ): (List[Char], ParsedToml) = match List.head(input) {
      case Some(c) => if (isWhiteSpace(c)) {
        parseWhiteSpaceAfterKey(List.drop(1, input), prefix, acc)
      } else if (c == '.') {
        parseKey(List.drop(1, input), prefix, acc)
      } else if (c == '=') {
        parseWhiteSpaceBeforeValue(List.drop(1, input), prefix, Nil, acc)
      } else {
        ???
      }
      case None => ???
    }

    def isCR(c: Char): Bool = c == Char.fromInt32(13)

    def isLF(c: Char): Bool = c == Char.fromInt32(10)

    def stripCRLF(input: List[Char]): List[Char] = match List.head(input) {
      case Some(c) => if (isLF(c)) {
        List.drop(1, input)
      } else {
        // There should always be a LF after CR
        ???
      }
      //Missing LF after CR
      case None => ???
    }

    def stripComment(input: List[Char]): List[Char] = match List.head(input) {
      case Some(c) => if (isLF(c)) {
        List.drop(1, input)
      } else if (isCR(c)) {
        stripCRLF(List.drop(1, input))
      } else if (isValidCommentChar(c)) {
        stripComment(List.drop(1, input))
      } else {
        ???
      }
      case None => input
    }

    def isWhiteSpace(c: Char): Bool = c == Char.fromInt32(9) || c == Char.fromInt32(32)

    def isKeyChar(c: Char): Bool =
      Char.isAscii(c) && (Char.isLetter(c) || Char.isDigit(c) || c == '_' || c == '-')

    def isValidCommentChar(c: Char): Bool = {
      let i = c as Int;
      !(i <= 8 || (10 <= i && i <= 31))
    }
}
