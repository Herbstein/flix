namespace ParseToml {
    type alias Parser[a] = Parser2.Parser[a]

    type alias ParsedToml = Map[List[Char], InnerToml]

    type alias TomlPath = List[List[Char]]

    pub enum InnerToml {
        case MapVal(ParsedToml),
        case ArrayTableVal(List[ParsedToml]),
        case StringVal(String),
        case DateVal(String),
        case FloatVal(String),
        case BoolVal(Bool),
        case IntVal(Int)
    }

    pub def serialize(input: ParsedToml): String = Map.foldWithKey(
        initial -> k -> v -> {
            initial + Char.toString('"')
                    + List.toString(k)
                    + Char.toString('"')
                    + " = "
                    + internalSerialize(v)
                    + Char.toString('\n')
        },
        "",
        input
    )

    def internalSerialize(input: InnerToml): String = match input {
        case StringVal(s) => Char.toString('"') + s + Char.toString('"')
        case _ => ???
    }


    pub def parse(input: List[Char]): Option[ParsedToml] = {
        use Parser2.{map, fold, tag, char, takeWhile};

        let parser = fold(
            parseTomlLine(),
            (Nil, Map.empty()),
            t -> f -> t.f()
        )
        .map(t -> {
            let (_, m) = t;
            m
        });

        match parser(input) {
            case Some((_, parsedToml)) => Some(parsedToml)
            case None => ???
        }
    }

    def parseTomlLine(): Parser[((TomlPath, ParsedToml)) -> (TomlPath, ParsedToml)] = {
        use Parser2.{map, or};
        parseKeyValue().map(input -> {
            let (k, v) = input;
            let f = tuple -> {
                let (p, m) = tuple;
                (p, Map.insert(k, v, m))
            };
            f
        }) `or` parseDictionaryContext().map(t -> {
            ???
        }) `or` parseArrayContext().map(_ -> {
            ???
        }) `or` parseWhiteLine().map(_ -> {
            t -> t
        })
    }

    def updateParsedToml(
        pt: ParsedToml,
        tp: TomlPath,
        f: ParsedToml -> Option[ParsedToml]
    ): Option[ParsedToml] = match tp {
        case Nil => pt.f()
        case x :: xs => match Map.get(x, pt) {
            case Some(v) => ???
            case None => Option.map(
                rest -> Map.insert(x, MapVal(rest), pt),
                updateParsedToml(Map.empty(), xs, f)
            )
        }
    }

    def updateInnerToml(
        toml: InnerToml,
        tp: TomlPath,
        f: ParsedToml -> Option[ParsedToml]
    ): Option[InnerToml] = match tp {
        case Nil => match toml {
            case MapVal(pt) => Option.map(x -> MapVal(x), pt.f())
            case _ => None
        } 
        case xs => match toml {
            case MapVal(pt) => Option.map(x -> MapVal(x), updateParsedToml(pt, xs, f))
            case ArrayTableVal(l) => Option.map(x -> ArrayTableVal(x :: l), updateParsedToml(Map.empty(), xs, f))
            case _ => None
        }
    }

    def parseWhiteLine(): Parser[()] = {
        use Parser2.{and, map};
        (ws() `and` newLine())
            .map(_ -> ())
    }

    def parseDictionaryContext(): Parser[List[List[Char]]] = {
        use Parser2.{delimit, tag};
        delimit(tag('['), parseKeys(), tag(']'))
    }

    def parseArrayContext(): Parser[List[List[Char]]] = {
        use Parser2.{delimit, tag, and};
        delimit(tag('[') `and` tag('['), parseKeys(), tag(']') `and` tag(']'))
    }

    pub def parseKeyValue(): Parser[(List[Char], InnerToml)] = {
        use Parser2.{and, tag, map, then};
        //Console.printLine("parseKeyValue");

        parseKey() `and` (ws() `and` tag('=') `and` ws()).then(_ -> parseValue())
    }

    def parseKeys(): Parser[List[List[Char]]] =
        use Parser2.{and, tag, takeWhile, map};
    (parseKey() `and` takeWhile(
        (ws() `and` tag('.') `and` ws() `and` parseKey())
            .map(t -> {
                let (_, k) = t;
                k
            })
    )).map(t -> {
        let (lhs, rhs) = t;
        lhs :: rhs
    })

    pub def parseKey(): Parser[List[Char]] = {
        use Parser2.or;

        parseBareKey() `or` parseLiteralString() `or` parseBasicString()
    }

    pub def parseValue(): Parser[InnerToml] = {
        use Parser2.map;

        parseString().map(List.toString).map(StringVal)
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

    pub def parseBasicString(): Parser[List[Char]] = {
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
        case '"' :: xs => None
        case x :: xs => Some((xs, x))
        case Nil => None
    }

    def newLine(): Parser[()] = {
        use Parser2.{char, filter, or, and, map};
        (char().filter(isCR) `and` char().filter(isLF))
            .map(_ -> ())
        `or` char().filter(isLF).map(_ -> ())
    }

    def ws(): Parser[()] = {
        use Parser2.{tag, takeWhile, or, map};
        takeWhile(tag(' ') `or` tag('\t')).map( _ -> ())
    }

    def isCR(c: Char): Bool = c == Char.fromInt32(13)

    def isLF(c: Char): Bool = c == Char.fromInt32(10)

    namespace Test {
        def ppBool(b: Bool): String = match b {
            case true => "true"
            case false => "false"
        }


        def test(name: String, f: () -> Bool & Impure): Bool & Impure = {
            Console.print("Testing ::: ");
            Console.print(name);
            Console.print(": ");
            let res = f();
            Console.printLine(ppBool(res));
            res
        }

        pub def allTests(): Bool & Impure = {
            // Bulder bulder
            let first = test("Simple test", simpleTest);
            first
        }

        def simpleTest(): Bool & Impure = {
            use ParseToml.{parse, InnerToml, serialize};

            let parsed = parse('a' :: ' ' :: '=' :: ' ' :: '"' :: 'b' :: '"' :: Nil);
            match parsed {
                case None => {
                    Console.printLine(
                        "Fuck, shit, fuck, hvad er det her for noget lort?!?!?!?!?"
                    );
                    false
                }
                case Some(outerToml) => {
                    Console.printLine(outerToml.serialize());
                    match Map.get(String.toList("a"), outerToml) {
                        case None => {false}
                        case Some(innerToml) => match innerToml {
                            case InnerToml.StringVal(s) => s == "b"
                            case _ => {
                                Console.printLine(outerToml.serialize());
                                false
                            }
                        }
                    }
                }
            }
        }
    }
}
