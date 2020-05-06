namespace ParseToml {
    type alias Parser[a] = Parser.Parser[a]

    type alias ParsedToml = Map[String, InnerToml]

    pub enum InnerToml {
        case MapVal(ParsedToml),
        case ArrayVal(List[InnerToml])
        case StringVal(String),
        case DateVal(String),
        case FloatVal(String),
        case BoolVal(Bool),
        case IntVal(Int)
    }

    enum SemanticToml {
        case Table(Map[String, SemanticToml])
        case ArrayOfTables(List[Map[String, SemanticToml]])
        case InlineTable(Map[String, SemanticToml])
        case ArrayV(List[SemanticToml])
        case StringV(String)
        case IntV(Int)
        case FloatV(String)
        case BoolV(Bool)
        case OffsetDateTime(String)
        case LocalDateTime(String)
        case LocalDate(String)
        case LocalTime(String)
    }

    pub def dummy(): List[SemanticToml] =
        Table(Map.empty()) ::
        ArrayOfTables(Nil) ::
        InlineTable(Map.empty()) ::
        ArrayV(Nil) ::
        StringV("") ::
        IntV(0) ::
        FloatV("") ::
        BoolV(false) ::
        OffsetDateTime("") ::
        LocalDateTime("") ::
        LocalDate("") ::
        LocalTime("") :: Nil

    enum TomlLine {
        case TableDef(List[String]),
        case ArrayTable(List[String]),
        case KeyValuePair(List[String], SemanticToml)
    }

    pub def serializeLines(input: List[TomlLine]): String = List.fold(
        (i, l) -> i + match l {
            case TableDef(k) => "[" + serializeKey(k) + "]"
            case ArrayTable(k) => "[[" + serializeKey(k) + "]]"
            case KeyValuePair(k, v) => serializeKey(k) + " = " + serializeSem(v)
        } + Char.toString('\n'), "Lines print!" + Char.toString('\n'), input)

    def serializeKey(input: List[String]): String = input |> String.intercalateChar('.')

    def serializeSem(input: SemanticToml): String = match input {
        case Table(map) => ???
        case ArrayOfTables(list) => ???
        case InlineTable(map) => ???
        case ArrayV(list) => "[" + (list |> List.map(e -> serializeSem(e)) |> String.intercalate(", ")) + "]"
        case StringV(str) => Char.toString('"') + str + Char.toString('"')
        case IntV(int) => ???
        case FloatV(str) => ???
        case BoolV(bool) => ???
        case OffsetDateTime(str) => ???
        case LocalDateTime(str) => ???
        case LocalDate(str) => ???
        case LocalTime(str) => ???
    }

    pub def serialize(input: ParsedToml): String = Map.foldWithKey(
        initial -> k -> v -> {
            initial + Char.toString('"')
                    + k
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
        case MapVal(t) => Char.toString('{') + serialize(t) + Char.toString('}')
        case _ => ???
    }

    pub def parse(input: List[Char]): Option[ParsedToml] & Impure = input |> parseTomlLines() |>
        Option.flatMap(match (_, tomlLines) -> {
            Console.printLine(serializeLines(tomlLines));
            parseHelper(tomlLines, Nil, Map.empty())
        }) |>
        Option.map(mapSem)

    /*pub enum InnerToml {
        case MapVal(ParsedToml),
        case ArrayTablesVal(List[ParsedToml]),
        case ArrayVal(List[InnerToml])
        case StringVal(String),
        case DateVal(String),
        case FloatVal(String),
        case BoolVal(Bool),
        case IntVal(Int)
    }*/

    def mapSem(input: Map[String, SemanticToml]): Map[String, InnerToml] = Map.foldWithKey(
        (initial, key, value) -> Map.insert(key, match value {
            case Table(map) => MapVal(mapSem(map))
            case ArrayOfTables(array) => ArrayVal(List.map(i -> MapVal(mapSem(i)), array))
            case InlineTable(map) => MapVal(mapSem(map))
            case ArrayV(array) => ArrayVal(List.map(i -> mapSemInner(i), array))
            case StringV(str) => StringVal(str)
            case IntV(int) => IntVal(int)
            case FloatV(str) => FloatVal(str)
            case BoolV(bool) => BoolVal(bool)
            case OffsetDateTime(str) => DateVal(str)
            case LocalDateTime(str) => DateVal(str)
            case LocalDate(str) => DateVal(str)
            case LocalTime(str) => DateVal(str)
        }, initial),
        Map.empty(),
        input
    )

    def mapSemInner(input: SemanticToml): InnerToml = match input {
            case Table(map) => MapVal(mapSem(map))
            case ArrayOfTables(array) => ArrayVal(
                List.reverse(List.map(i -> MapVal(mapSem(i)), array))
            )
            case InlineTable(map) => MapVal(mapSem(map))
            case ArrayV(array) => ArrayVal(List.map(i -> mapSemInner(i), array))
            case StringV(str) => StringVal(str)
            case IntV(int) => IntVal(int)
            case FloatV(str) => FloatVal(str)
            case BoolV(bool) => BoolVal(bool)
            case OffsetDateTime(str) => DateVal(str)
            case LocalDateTime(str) => DateVal(str)
            case LocalDate(str) => DateVal(str)
            case LocalTime(str) => DateVal(str)
    }

    //def convertInt(int: String): Int = ???

    //def convertBool(bool: String): Bool = ???

    def parseHelper(
        input: List[TomlLine],
        table: List[String],
        outer: Map[String, SemanticToml]
    ): Option[Map[String, SemanticToml]] & Impure = match input {
        case Nil => Some(outer)
        case TableDef(key) :: xs => match List.reverse(key) {
            case last :: init =>
                indexTable(List.reverse(init), outer) |> Option.flatMap(
                    inner -> match inner |> Map.get(last) {
                        case Some(_) => None
                        case None => {
                            inner |>
                                Map.insert(last, Table(Map.empty())) |>
                                insertTable(init, outer) |>
                                parseHelper(xs, key)
                        }
                    }
                )
            case Nil =>
                // Unreachable: It's a parse error to find a Table/ArrayOfTables definition without
                // any keys
                None

        }
        case ArrayTable(key) :: xs => match List.reverse(key) {
            case last :: init =>
                indexTable(List.reverse(init), outer) |>
                Option.flatMap(t -> match Map.get(last, t) {
                    case Some(ArrayOfTables(array)) =>
                        insertTable(
                            List.reverse(init),
                            outer,
                            Map.update(_ -> Some(ArrayOfTables(Map.empty() :: array)), last, t)
                        ) |> parseHelper(xs, key)
                    case Some(_) =>
                        // Incompatible type
                        None
                    case None =>
                        insertTable(
                            List.reverse(init),
                            outer,
                            Map.insert(last, ArrayOfTables(Map.empty() :: Nil), t)
                        ) |> parseHelper(xs, key)
                })
            case Nil =>
                // Unreachable: See TableDef case
                None
        }
        case KeyValuePair(_) :: _ => 
            indexTable(table, outer) |>
            Option.flatMap(inner -> populateTable(input, inner)) |>
            Option.flatMap(match (updatedInput, updatedInner) ->
                parseHelper(updatedInput, table, insertTable(table, outer, updatedInner))
            )
    }

    // Inserts inner at the position described by keys. If that's somehow impossible, the original
    // map will be returned.
    def insertTable(
        keys: List[String],
        outer: Map[String, SemanticToml],
        inner: Map[String, SemanticToml]
    ): Map[String, SemanticToml] = match keys {
        case Nil => inner
        case k :: ks => match outer |> Map.get(k) {
                case None => Map.insert(k, Table(insertTable(ks, Map.empty(), inner)), outer)
                case Some(Table(nested)) =>
                    Map.update(_ -> Some(Table(insertTable(ks, nested, inner))), k, outer)
                case Some(ArrayOfTables(nested)) => match nested {
                    case x :: xs =>
                        Map.update(_ ->
                            Some(ArrayOfTables(insertTable(ks, x, inner) :: xs)),
                            k, outer
                        )
                    case _ => outer
                }
                case _ => outer
            }
    }
    
    def indexTable(
        keys: List[String],
        outer: Map[String, SemanticToml]
    ): Option[Map[String, SemanticToml]] & Impure = match keys {
        case Nil => Some(outer)
        case k :: ks => match outer |> Map.get(k) {
            case Some(Table(t)) => indexTable(ks, t)
            case Some(ArrayOfTables(l)) => ???
            case None => Some(Map.empty())
            case Some(_) => None
        }
    }

    // If the Table has not been made while beeing in the current Table, Error! This is _not_
    // currently correctly implemented.
    def populateTable(
        input: List[TomlLine],
        acc: Map[String, SemanticToml]
    ): Option[(List[TomlLine], Map[String, SemanticToml])] = match input {
        // This would be the place for cleaning up any markers, saying that tables have been
        // inserted while being in this table definition, and therefore can be appended to.
        // TODO:
        case TableDef(_) :: _ => Some((input, acc))
        case ArrayTable(_) :: _ => Some((input, acc))
        case KeyValuePair(k, v) :: xs =>
            insert(k, v, acc) |> Option.flatMap(updated -> populateTable(xs, updated))
        case Nil => Some((input, acc))
    }

    def insert(
        key: List[String],
        value: SemanticToml,
        acc: Map[String, SemanticToml]
    ): Option[Map[String, SemanticToml]] = match key { 
        case k :: x :: xs => 
            match acc |> Map.get(k) {
                case Some(inner) => {
                    // As this table have been created before we should reject it unless it's
                    // marked as appendable.
                    match inner {
                        case Table(t) => insert(x :: xs, value, t) |> Option.map(
                            updated -> acc |> Map.update(_ -> Some(Table(updated)), k))
                        case ArrayOfTables(t) =>
                            // TODO:
                            ???
                        case _ => None
                    }
                }
                case None =>
                    insert(xs, value, Map.empty()) |> Option.map(updated -> 
                        Map.insert(k, Table(updated), acc)
                    )
            }
        case k :: Nil => {
            // Yay we are almost done, just need to insert the value, and everything is gonna be
            // allright.
            match Map.get(k, acc) {
                case None => Some(Map.insert(k, value, acc))
                case Some(_) => None
            }
        }
        case Nil => {
            // We have recieved an empty key, that's illegal (and should have been caught earlier in
            // the parser anyway, so don't know what's going on..)
            None
        }

    }
    
    def parseTomlLines(): Parser[List[TomlLine]] =
        use Parser.{takeWhile, and, map, then};
        (parseTomlLine() `and` takeWhile(parseNewLine().then(_ -> parseTomlLine())))
            .map(match (first, rest) -> first :: rest)

    def parseTomlLine(): Parser[TomlLine] =
        use Parser.{or, map, then};
        parseNewLine().then(_ -> parseTomlLine()) `or`
        parseTable() `or`
        parseArrayOfTables() `or`
        parseKeyValue()

    def parseNewLine(): Parser[()] = {
        use Parser.{and, map};
        (ws() `and` newLine())
            .map(_ -> ())
    }

    def parseTable(): Parser[TomlLine] = {
        use Parser.{delimit, tag, map};
        delimit(tag('['), parseKeys(), tag(']')).map(TableDef)
    }

    def parseArrayOfTables(): Parser[TomlLine] = {
        use Parser.{delimit, tag, and, map};
        delimit(tag('[') `and` tag('['), parseKeys(), tag(']') `and` tag(']')).map(ArrayTable)
    }

    def parseKeyValue(): Parser[TomlLine] =
        use Parser.{and, tag, map, then};

        ((parseKeys() `and` (ws() `and` tag('=') `and` ws())).map(match (k, _) -> k)
            `and` parseValue()).map(match (k, v) -> KeyValuePair(k, v))

    def parseKeys(): Parser[List[String]] =
        use Parser.{and, tag, takeWhile, map};
        (parseKey() `and`
            takeWhile(
                (ws() `and` (tag('.')) `and` (ws()) `and` parseKey())
                    .map(match (_, k) -> k))
        ).map(match (lhs, rhs) -> lhs :: rhs)

    pub def parseKey(): Parser[String] = {
        use Parser.or;

        parseBareKey() `or` parseLiteralString() `or` parseBasicString()
    }

    pub def parseValue(): Parser[SemanticToml] = {
        use Parser.map;

        parseString().map(StringV) /*`or`
        parseInt().map(v -> IntV(convertInt(v))) `or`
        parseBool().map(v -> BoolV(convertBool(v)))*/
    }


    def parseBareKey(): Parser[String] = {
        use Parser.{takeWhile, or, tag, map};
        takeWhile(parseDigit() `or` parseLetter() `or` tag('-') `or` tag('_'))
            .notNil()
            .map(List.toString)
    }

    def parseString(): Parser[String] = {
        use Parser.or;

        parseMultiLineBasicString() `or`
            parseMultiLineLiteralString() `or`
            parseLiteralString() `or`
            parseBasicString()
    }

    def parseLiteralString(): Parser[String] = {
        use Parser.{delimit, char, filter, takeWhile, tag, map};
        let quote = tag(Char.fromInt32(39));
        delimit(quote, takeWhile(char().filter(c -> c != '\'' && !c.isCR() && !c.isLF())), quote).map(List.toString)
    }

    def parseMultiLineLiteralString(): Parser[String] = {
        use Parser.{then, not, delimit, and, char, takeWhile, tag, map};
        let quote = tag('\'') `and` tag('\'') `and` tag('\'');
        delimit(quote, takeWhile(not(quote).then(() -> char())), quote).map(List.toString)
    }

    pub def parseBasicString(): Parser[String] = {
        use Parser.{tag, takeWhile, delimit, map};

        delimit(tag('"'), takeWhile(parseEscapeChar()), tag('"')).map(List.toString)
    }

    def parseMultiLineBasicString(): Parser[String] = {
        use Parser.{tag, takeWhile, delimit, and, or, map};
        let quote = tag('"') `and` tag('"') `and` tag('"');

        delimit(quote, takeWhile(parseEscapeChar() `or` tag('\n') `or` tag('\r')), quote).map(List.toString)
    }

    def parseDigit(): Parser[Char] = {
        use Parser.{char, filter};

        char().filter(Char.isDigit)
    }

    def parseLetter(): Parser[Char] = {
        use Parser.{char, filter};

        char().filter(Char.isLetter)
    }

    def parseEscapeChar(): Parser[Char] = input -> match input {
        case '\\' :: xs => match xs {
            case 'u' :: u1 :: u2 :: u3 :: u4 :: ys => ???
            case 'U' :: u1 :: u2 :: u3 :: u4 :: u5 :: u6 :: u7 :: u8 :: ys => ???
            case 'b' :: ys => Some((ys, Char.fromInt32(8)))
            case 't' :: ys => Some((ys, Char.fromInt32(9)))
            case 'n' :: ys => Some((ys, Char.fromInt32(10)))
            case 'f' :: ys => Some((ys, Char.fromInt32(12)))
            case 'r' :: ys => Some((ys, Char.fromInt32(13)))
            case '"' :: ys => Some((ys, '"'))
            case '\\' :: ys => Some((ys, '\\'))
            case _ => None
        }
        case '"' :: xs => None
        case x :: xs => Some((xs, x))
        case Nil => None
    }

    def notNil(
        p: Parser[List[a]]
    ): Parser[List[a]] =
        use Parser.filter;
        p.filter(l -> !List.isEmpty(l))

    def newLine(): Parser[()] = {
        use Parser.{char, filter, or, and, map};
        (char().filter(isCR) `and` char().filter(isLF))
            .map(_ -> ())
        `or` char().filter(isLF).map(_ -> ())
    }

    def ws(): Parser[()] = {
        use Parser.{tag, takeWhile, or, map, not, filter, char};
        takeWhile(tag(' ') `or` char().filter(c -> c == Char.fromInt32(9))).map( _ -> ())
    }

    def isCR(c: Char): Bool = c == Char.fromInt32(13)

    def isLF(c: Char): Bool = c == Char.fromInt32(10)

    namespace Test {
        @test
        pub def simpleTest(): Bool & Impure =
            use ParseToml.{parse, InnerToml, serialize};

            Console.printLine("");
            Console.printLine("simpleTest");

            let parsed = ParseToml.parse('a' :: ' ' :: '=' :: ' ' :: '"' :: 'b' :: '"' :: Nil);
            match parsed {
                case None => false
                case Some(outerToml) => match Map.get("a", outerToml) {
                    case None => false
                    case Some(innerToml) => match innerToml {
                        case ParseToml/InnerToml.StringVal(s) => s == "b"
                        case _ => false
                    }
                }
            }

        @test
        pub def tableTest(): Bool & Impure =
            use ParseToml.{parse, InnerToml, serialize};
            Console.printLine("");
            Console.printLine("tableTest");

            let parsed = ParseToml.parse(String.toList("[a]"));

            match parsed {
                case Some(outer) => {
                    Console.printLine(ParseToml.serialize(outer));
                    match outer |> Map.get("a") {
                        case Some(ParseToml/InnerToml.MapVal(_)) => true
                        case _ => false
                    }
                }
                case _ => false
            }

        @test
        pub def moreComplicated(): Bool & Impure = {
            use ParseToml.{parse, InnerToml, serialize};
            Console.printLine("");
            Console.printLine("moreComplicated");
            let str = String.intercalateChar(Char.fromInt32(10),
                "[a]" ::
                "a = 'a'" :: Nil
            );
            //Console.printLine(str);
            let parsed = ParseToml.parse(String.toList(str));
            match parsed {
                case Some(outer) => {
                    Console.printLine(ParseToml.serialize(outer));
                    match Map.get("a", outer) {
                        case Some(ParseToml/InnerToml.MapVal(table)) => match Map.get("a", table) {
                            case Some(ParseToml/InnerToml.StringVal("a")) => true
                            case _ => {Console.printLine("inner"); false}
                        }
                        case _ => {Console.printLine("table"); false}
                    }
                }
                case _ => false
            }
        }

        @test
        pub def arrayTest(): Bool & Impure = {
            use ParseToml.{parse, InnerToml, serialize};
            let str = String.intercalateChar(Char.fromInt32(10),
                "[a]" ::
                "a = ['a', 'a']" :: Nil
            );
            //Console.printLine(str);
            let parsed = ParseToml.parse(String.toList(str));
            match parsed {
                case Some(outer) => {
                    Console.printLine(ParseToml.serialize(outer));
                    match Map.get("a", outer) {
                        case Some(ParseToml/InnerToml.MapVal(table)) => match Map.get("a", table) {
                            case Some(ParseToml/InnerToml.ArrayVal(ParseToml/InnerToml.StringVal("a") :: ParseToml/InnerToml.StringVal("a") :: Nil)) => true
                            case _ => {Console.printLine("inner"); false}
                        }
                        case _ => {Console.printLine("table"); false}
                    }
                }
                case _ => false
            }
        }
    }
}
