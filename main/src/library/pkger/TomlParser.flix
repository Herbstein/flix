namespace Toml {
    type alias ParsedToml = Map[String, InnerToml]

    pub enum InnerToml {
        case MapVal(ParsedToml),
        case ArrayTableVal(List[ParsedToml]),
        case StringVal(String),
        case DateVal(String),
        case FloatVal(String),
        case BoolVal(Bool),
        case IntVal(Int)
    }

    def internalInternalParser(input: String): ParsedToml = ???
    
    def parseString(input: String): Parser[String] = match String.charAt(0, input) {
        case '\'' => Parser.char('\'') >>= Parser.span(c -> c != '\'') >>= Parser.char('\'')
        case _ => ???
    }

    def stripWhiteSpace(input: List[Char]): List[Char] = match List.head(input) {
        case Some(c) => ???
        case None => ???
    }

    def isCR(c: Char): Bool = c == Char.fromInt32(13)

    def isLF(c: Char): Bool = c == Char.fromInt32(10)

    def isWhiteSpace(c: Char): Bool = c == Char.fromInt32(9) || c == Char.fromInt32(32)

    def isKeyChar(c: Char): Bool =
      Char.isAscii(c) && (Char.isLetter(c) || Char.isDigit(c) || c == '_' || c == '-')

    def isValidCommentChar(c: Char): Bool = {
      let i = c as Int;
      !(i <= 8 || (10 <= i && i <= 31))
    }

    /*
    def insert(
        original: ParsedToml,
        path: Array[String],
        element: ParsedToml
    ): ParsedToml = Map.insertWith(v -> v -> insert(

    def merge(lhs: ParsedToml, rhs: ParsedToml): ParsedToml = Map.unionWith(
        l -> r -> {
            match l {
                case MapVal(p) =>
        }, lhs, rhs)

    match original {
        case MapVal(m) => if (Map.exists(k -> v -> {k == List.head(path)}, m)) {
            Map.update(v -> insert(v, 
            
        case ArrayTableVal(a) => ???
        case StringVal(_) => ???
        case DateVal(_) => ???
        case FloatVal(_) => ???
        case BoolVal(_) => ???
        case IntVal(_) => ???
    }

    enum ParsedTable {
        case ArrayTable(String),
        case Table(String)
    }

    /// Recursive decent parser of TOML. Will crash if invalid TOML is
    /// encountered.

    pub def parse(input: String): ParsedToml = ???

    def internalParse(
            input: List[Char],
            table: List[String],
            acc: ParsedToml
    ): (List[Char], ParsedToml) = match List.head(input) {
        case Some(c) => if (isWhiteSpace(c)) {
            let rec = stripWhiteSpace(List.drop(1,input));
            internalParse(rec, table, acc)
        } else if (isLF(c)) {
            let rec = stripNewlines(List.drop(1, input));
            internalParse(rec, table, acc)
        } else if (isCR(c)) {
            let rec = stripNewlinesCR(List.drop(1, input));
            internalParse(rec, table, acc)
        } else if (c == '#') {
            let rec = stripComment(List.drop(1, input));
            internalParse(rec, table, acc)
        } else if (c == '[') {
            let rec = parseTable(input);
            match rec.snd() {
                case Table(xs) => internalParse(rec.fst(), xs, insertTable(xs))
                case ArrayTable(xs) => internalParse(rec.fst(), xs, pushArrayTable(xs))
            }
        } else if (c == '"' || c == Char.fromInt32(39) || isKeyChar(c)) {
            parseKeyValuePair(input, table, acc)
        } else {
            ???
        }
        case None => acc
    }


    def stripNewlines(input: List[Char]): List[Char] = match List.head(input) {
        case Some(c) => if (isLF(c)) {
            stripNewlines(List.drop(1, input))
        } else if (isCR(c)) {
            stripNewlinesCR(List.drop(1, input))
        } else {
            ???
        }
        case None => ???
    }

    def stripNewlinesCR(input: List[Char]): List[Char] = match List.head(input) {
        case Some(c) => if (isLF(c)) {
            stripNewLines(List.drop(1, input))
        } else {
            ???
        }
        case None => ???
    }

    def parseTable(input: List[Char]): ParsedToml = match List.head(input) {
        case Some(c) => ???
        case None => ???
    }

    def pushArrayTable(
        parsed: ParsedToml,
        newTableEntry: List[String]
    ): ParsedToml = 
    */
    /*
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
    */
}
