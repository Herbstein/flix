namespace ParseToml {
    //Temporary definition while standard  library doesn't provide it yet:
    def init(xs: List[a]): Option[List[a]] = initHelper(xs, Nil)


    def initHelper(xs: List[a], acc: List[a]): Option[List[a]] = match xs {
        case x :: Nil => Some(List.reverse(acc))
        case Nil => None
        case x :: rs => initHelper(rs, x :: acc)
    }

    type alias Parser[a] = Parser2.Parser[a]

    type alias ParsedToml = Map[String, InnerToml]

    type alias TomlPath = List[String]

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

    enum ParsingToml {
        case ITableVal(ParsingToml),
        case ITableVar(ParsingToml),
        case IArrayVal(List[ParsingToml]),
        case IArrayVar(List[ParsingToml]),
        case IStringVal(String),
        case IDateVal(String),
        case IFloatVal(String),
        case IBoolVal(Bool),
        case IIntVal(Int)
    }

    def toInnerToml(parsed: ParsingToml): InnerToml =
        match parsed {
            case ITableVal(map) => TableVal(map |> convertTable())
            case ITableVar(map) => TableVal(map |> convertTable())
            case IArrayVal(array) => ArrayVal(array.convertArray(Nil))
            case IArrayVar(array) => ArrayVal(array.convertArray(Nil))
            case IStringVal(str) => StringVal(str)
            case IDateVal(str) => DateVal(str)
            case IFloatVal(str) => FloatVal(str)
            case IBoolVal(b) => BoolVal(b)
            case IIntVal(i) => IntVal(i)
        }

    def convertTable(parsed: Map[String, ParsingToml]): Map[String, InnerToml] =
        use Map.map;
        parsed |> map(p -> p.toInnerToml())

    def convertArray(parsed: List[ParsingToml]): List[InnerToml] = convertArrayLoop(parsed, Nil)

    def convertArrayLoop(parsed: List[ParsingToml], acc: List[InnerToml]): List[InnerToml] = 
        match parsed {
            case x :: xs => x.toInnerToml() :: acc |> convertArrayLoop(xs)
            case Nil => 
                // We don't need to reverse acc, since the elements are in reverse order. Because of
                // this the array just been fixed up.
                acc
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
    pub def parse(input: List[Char]): Result[ParsedToml, ParseError] =
        parseTableLoop(input, ITableVal(Map.empty())) |> flatMap(toInnerToml)
    

    def parseTableLoop(input: List[Char], acc: ParsingToml): ParsingToml = {
        use List.last;
        //use List.init;
        use Result.flatMap;
        parseInternalLoop(input, acc) |> flatMap(match
        (input1, parsed) -> 
        match input1 {
            case '[' :: xs => {
                let trimmed = xs.trimSpace();
                match trimmed {
                    case '[' :: xss => {
                        parseKey(xss) |> flatMap(match
                        (xsss, keys) ->
                        match keys.init() {
                            case Some(path) => {
                                indexNestedTable(path) |> flatMap(
                                innerMap -> match innerMap |> get(keys.last()) {
                                    case ArrayVar => {
                                        
                                    }
                                })   
                            }
                            case None => ???
                        })
                    }
                    case _ => {
                        // We have encountered a dictionary defenition, first we need to follow the
                        // path it is pointing at to be able to use it. 

                        // Since we in the case of dictionaries don't have to insert anything
                        // special at the end, we can just use the indexNestedTable to return our
                        // exact expected position.
                        parseKey(trimmed) |> flatMap(match
                        (xss, keys) -> 
                        indexNestedTable(acc, keys) |> flatMap(
                        innerMap ->
                        parseInternalLoop(xss, innerMap) |> flatMap(match

                        // New when we are done creating a updated inner map, we can insert the
                        // updated map in acc.
                        (xsss, updatedInner) -> 

                        insertNestedTable(acc, updatedInner, keys) |> flatMap(
                        updatedOuter ->
                        parse(xsss, updatedOuter)
                        ))))
                    }
                }
            }
            case _ :: xs => {
                // We are always called after parseInternal has run. That only returns if it either
                // saw a '[' or EOF. Because of this we shouldn't be able to reach this point.
                Err("Unreachable!")
            }
            case Nil => Ok(parsed)
        })
    }
    
    // Parses key-value pairs until a new table or array-of-table definition is reached. Returns the
    // rest of the input, or an error descriping what went wrong.
    def parseInternalLoop(
        input: List[Char],
        acc: ParsedToml
    ): Result[(List[Char], ParsedToml), ParseError] = {
        use Result.flatMap;
        let trimmed = input.trimSpace();
        match trimmed {
            case '[' :: xs => Ok((trimmed, acc))
            case '#' :: xs => xs.trimLine() |> flatMap(
                                  xss -> parseInternalLoop(xss, acc)
                              )
            case x :: xs => {
                parseKey(input) |> flatMap(match
                (ys, keys) -> 
                let xss = ys.trimSpace();
                match xss {
                    case '=' :: zs => {
                        let ls = zs.trimSpace();
                        parseValue(ls) |> flatMap(match
                        (cs, value) -> 
                            Err("Unimplemented!")
                        )
                    }
                    case c :: _ => Err("Expected '=' after key, encountered " + Char.toString(c))
                    case Nil => Err("Encountered EOF after key decleration")
                })
            }
            case Nil => Ok((Nil, acc))
        }
    }

    // Parses optionally dot-seperated keys until a character which cannot be the start of a key is
    // reached. Returns the input with the parsed keys and any following whitespace removed, and a
    // list of parsed keys, or an error if it wasn't possible to parse any key.
    def parseKey(input: List[Char]): Result[(List[Char], List[String]), ParseError] = ???

    // Infers a toml value and its type from a string. Returns the input with the parsed value and
    // any following whitespace removed, and the inferred value or an error descriping what went
    // wrong if it wasn't possible to extract a value.
    def parseValue(input: List[Char]): Result[(List[Char], InnerToml), ParseError] = ???

    // Inserts a value in the Map at the position given in the path. If an `InnerToml.ArrayVal` is
    // encountered on the way, the new element will be inserted in the map at the bottom of the
    // list. Returns a updated `ParsedToml` or an error if one of the keys in the path didn't lead
    // to either a `InnerToml.MapVal` or `InnerToml.ListVal`.
    def insertNested(
        map: ParsedToml,
        insert: InnerToml,
        path: List[String]
    ): Result[ParsedToml, ParseError] = ???

    // Adds a `ParsedToml` to the array at the position given in the path. Returns a updated
    // `ParsedToml` or an error if the path doesn't lead to an `InnerToml.ArrayVal`, or if one of
    // the keys in the path doesn't lead to a `InnerToml.MapVal` or `InnerToml.ListVal`
    def insertNestedArray(
        map: ParsedToml,
        inner: ParsedToml,
        path: List[String]
    ): Result[ParsedToml, ParseError] = ???

    // This might be functionally the same as insertNested, execpt we know the outermost
    // datastructure is a map. Might be worth it to change the one caller to insertNested
    def insertNestedTable(
        map: ParsedToml, 
        inner: ParsedToml,
        path: List[String]
    ): result[ParsedToml, ParseError] = ???

    // Follows a `Map[String, InnerToml]` layer for layer, creating new internal hashmaps if no
    // entry is present. Returns the map encountered after following all keys, or an `Err` with
    // an explanation if a type other than `InnerToml.MapVal` or `InnerToml.ArrayVal` was
    // encountered on the way.
    def indexNestedTable(
        map: ParsedToml,
        path: List[String]
    ): Result[ParsedToml, ParseError] = ???

    def parseEscapeChar(input: List[Char]): Option[(List[Char], Char)] = match input {
            case 'u' :: u1 :: u2 :: u3 :: u4 :: ys => ???
            case 'U' :: u1 :: u2 :: u3 :: u4 :: u5 :: u6 :: u7 :: u8 :: ys => ???
            case 'b' :: ys => ???/*Some(ys, ???)*/
            case 't' :: ys => Some((ys, '\t'))
            case 'n' :: ys => Some((ys, '\n'))
            case 'f' :: ys => ???/*Some(ys, ???)*/
            case 'r' :: ys => Some((ys, '\r'))
            case '"' :: ys => Some((ys, '"'))
            case '\\' :: ys => Some((ys, '\\'))
            case _ => None
    }

    def trimLine(input: List[Char]): Result[List[Char], ParseError] = {
        match input {
            case x :: xs => if (isCR(x)) {
                match input {
                    case y :: ys => if (isLF(x)) {
                        Ok(ys)
                    } else {
                        Err("Unfinished CR LF sequence, this looks like a broken file")
                    }
                    case Nil => Ok(Nil)
                }
            } else if (isLF(x)) {
                Ok(xs)
            } else {
                trimLine(xs)
            }
            case Nil => Ok(Nil)
        }
    }

    def trimSpace(input: List[Char]): List[Char] = match input {
        case x :: xs => if (isWhiteSpace(x)) {
            trimSpace(xs)
        } else {
            xs
        }
        case Nil => input
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
        def simpleTest(): Bool = {
            use ParseToml.{parse, InnerToml};

            match parse('a' :: ' ' :: '=' :: ' ' :: '"' :: 'b' :: '"' :: Nil, Map.empty()) {
                case Ok(parsed) => match Map.get(String.toList("a"), parsed) {
                    case Some(InnerToml.StringVal(s)) => s == "b"
                    case _ => false
                }
                case _ => false
            }
        }
    }
}
