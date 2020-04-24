namespace ParseToml {
    type alias Parser[a] = Parser2.Parser[a]

    type alias ParsedToml = Map[List[Char], InnerToml]

    type alias TomlPath = List[List[Char]]

    type alias ParseError = String

    type alias ParseResult = Result[ParsedToml, ParseError]

    pub enum InnerToml {
        case TableVal(ParsedToml),
        case ArrayVal(List[ParsedToml]),
        case StringVal(String),
        case DateVal(String),
        case FloatVal(String),
        case BoolVal(Bool),
        case IntVal(Int)
    }

    def internalSerialize(input: InnerToml): String = match input {
        case StringVal(s) => Char.toString('"') + s + Char.toString('"')
        case _ => ???
    }


    /**
     * input: A list of characters expected to be a toml format
     * acc: A empty map to be populate with a toml structure
     * returns:
     *     Ok: A Map from strings to InnerToml representing the input string
     *     Err: A String descriping what went wrong
     */
    pub def parse(
        input: List[Char],
        // This may have to be a TomlPath, for ArrayTables to work properly
        acc: ParsedToml
    ): Result[ParsedToml, ParseError] = {
        match parseInternal(input, acc) {
            case (input1, Ok(parsed)) => {
                match input1 {
                    case '[' :: xs => {
                        let trimmed = xs.trimSpace();
                        match trimmed {
                            case '[' :: xss => match parseKey(xsss) {
                                case (xsss, Ok(keys)) => 
                                case (_, Err(s)) => s
                            }
                            case _ => match parseKey(trimmed) {
                                case (xss, Ok(keys)) => ???
                                case (_, Err(s)) => ???
                            }
                        } 
                    }
                    case _ :: xs => {
                        // We are always called after parseInternal has run. That only returns if
                        // it either saw a '[' or EOF. Because of this we shouldn't be able to
                        // reach this point.
                        Err("Unreachable!")
                    }
                    case Nil => Ok(parsed)
                }
            }
            case (_, Err(s)) => Err(s)
        }
    }

    def parseInternal(
        input: List[List[Char]],
        acc: ParsedToml
    ): (input, Result[ParsedToml, ParseError]) = ???


    def insertNested(
        map: ParsedToml,
        insert: InnerToml,
        path: List[List[Char]]
    ): Result[ParsedToml, ParseError] = ???


        

    def parseEscapeChar(input: List[Char]): (List[Char], Option[Char]) = input -> match input {
        match input {
            case 'u' :: u1 :: u2 :: u3 :: u4 :: ys => ???
            case 'U' :: u1 :: u2 :: u3 :: u4 :: u5 :: u6 :: u7 :: u8 :: ys => ???
            case 'b' :: ys => (Some(ys), ???)
            case 't' :: ys => (Some(ys), '\t')
            case 'n' :: ys => (Some(ys), '\n')
            case 'f' :: ys => (Some(ys), ???)
            case 'r' :: ys => (Some(ys), '\r')
            case '"' :: ys => (Some(ys), '"')
            case '\\' :: ys => (Some(ys), '\\')
            case _ => None
        }
    }

    def trimNewLine(): (List[Char], Option[()]) = {
        match input {
            case x :: xs => if (isCR(x)) {
                match input {
                    case y :: ys => if (isLF(x)) {
                        (Some(()), ys)
                    } else {
                        (None, input)
                    }
                    case Nil => (None, xs)
                }
            } else if (isLF(x)) {
                (Some(()), xs)
            } else {
                (None, input)
            }
            case Nil => (None, input)
        }
    }

    def trimWhiteSpace(input: List[Char]): (List[Char], Option[()]) = {
        match input {
            case x :: xs => if (isWhiteSpace(x)) {
                trimWhiteSpaceInternal(xs)
            } else {
                (xs, None)
            }
            case Nil => (input, None)
        }
    }

    def trimWhiteSpaceInternal(input: List[Char]): (List[Char], Option[()]) = {
        match input {
            case x :: xs => if (isWhiteSpace(x)) {
                trimWhiteSpaceInternal(xs)
            } else {
                (xs, Some(()))
            }
            case Nil => (input, Some(()))
        }
    }

    def isWhiteSpace(x: Char): Bool = {
        x == ' ' || x == '\t'
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

        @test
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
