namespace ParseToml {
    type alias Parser[a] = Parser2.Parser[a]

    type alias ParsedToml = Map[List[Char], InnerToml]

    pub enum InnerToml {
        case MapVal(ParsedToml),
        case ArrayTableVal(List[ParsedToml]),
        case StringVal(String),
        case DateVal(String),
        case FloatVal(String),
        case BoolVal(Bool),
        case IntVal(Int)
    }

    def newFold(
        p: Parser[(List[Char], InnerToml)],
        initial: ParsedToml,
        f: ParsedToml -> (List[Char], InnerToml) -> ParsedToml
    ): Parser[ParsedToml] = Parser2.fold(p, initial, f)

    /*pub def parse(input: List[Char]): Option[ParsedToml] = {
        use Parser2.{fold, tag, char, takeWhile};

        let parser = takeWhile(char());
        //let parser = parseKeyValue();
        let parser = newFold(
            char(),
            Map.empty(),
            _ -> _ -> ???
        );
        match parser(input) {
            case Some((_, parsedToml)) => Some(parsedToml)
            case None => ???
        }
        ???
    }*/


    pub def parseKeyValue(): Parser[(List[Char], InnerToml)] = {
        use Parser2.{and, tag, map, then};
        parseKey() `and` (ws() `and` tag('=') `and` ws()).then(_ -> parseValue())
    }

    def parseKey(): Parser[List[Char]] = {
        use Parser2.or;

        parseBareKey() `or` parseLiteralString() `or` parseBasicString()
    }

    def parseValue(): Parser[InnerToml] = {
        use Parser2.map;

        parseString().map(StringVal)
    }

    def parseBareKey(): Parser[List[Char]] = {
        use Parser2.{takeWhile, or, tag};
        takeWhile(parseDigit() `or` parseLetter() `or` tag('-') `or` tag('_'))
    }

    def parseString(): Parser[List[Char]] = {
        use Parser2.or;

        parseMultiLineBasicString() `or`
            parseMultiLineLiteralString() `or`
            parseLiteralString() `or`
            parseBasicString()
    }

    def parseLiteralString(): Parser[List[Char]] = {
        use Parser2.{delimit, char, filter, takeWhile, tag};
        let quote = tag('\'');
        delimit(quote, takeWhile(char().filter(c -> c != '\'' && !c.isCR() && !c.isLF())), quote)
    }

    def parseMultiLineLiteralString(): Parser[List[Char]] = {
        use Parser2.{then, not, delimit, and, char, takeWhile, tag};
        let quote = tag('\'') `and` tag('\'') `and` tag('\'');
        delimit(quote, takeWhile(not(quote).then(() -> char())), quote)
    }

    def parseBasicString(): Parser[List[Char]] = {
        use Parser2.{tag, takeWhile, delimit};
        delimit(tag('"'), takeWhile(parseEscapeChar()), tag('"'))
    }

    def parseMultiLineBasicString(): Parser[List[Char]] = {
        use Parser2.{tag, takeWhile, delimit, and, or};
        let quote = tag('"') `and` tag('"') `and` tag('"');

        delimit(quote, takeWhile(parseEscapeChar() `or` tag('\n') `or` tag('\r')), quote)
    }

    def parseDigit(): Parser[Char] = {
        use Parser2.{char, filter};

        char().filter(Char.isDigit)
    }

    def parseLetter(): Parser[Char] = {
        use Parser2.{char, filter};

        char().filter(Char.isLetter)
    }

    def parseEscapeChar(): Parser[Char] = input -> match input {
        case '\\' :: xs => match xs {
            case 'u' :: u1 :: u2 :: u3 :: u4 :: ys => ???
            case 'U' :: u1 :: u2 :: u3 :: u4 :: u5 :: u6 :: u7 :: u8 :: ys => ???
            case 'b' :: ys => Some((ys, ???))
            case 't' :: ys => Some((ys, '\t'))
            case 'n' :: ys => Some((ys, '\n'))
            case 'f' :: ys => Some((ys, ???))
            case 'r' :: ys => Some((ys, '\r'))
            case '"' :: ys => Some((ys, '"'))
            case '\\' :: ys => Some((ys, '\\'))
            case _ => None
        }
        case x :: xs => Some((xs, x))
        case Nil => None
    }

    def ws(): Parser[()] = {
        use Parser2.{tag, takeWhile, or, map};
        takeWhile(tag(' ') `or` tag('\t')).map( _ -> ())
    }

    def isCR(c: Char): Bool = c == Char.fromInt32(13)

    def isLF(c: Char): Bool = c == Char.fromInt32(10)
}
