enum TomlLine {
    case Table(List[String])
    case ArrayOfTable(List[String])
    case KeyValuePair((List[String], String))
}

def parseTableLoop(input: List[Char], acc: List[TomlLine]): Result[List[TomlLine], String] =
    use Result.flatMap;
    parseInternalLoop(input, acc) |> flatMap(match
    (input1, acc1) ->
    match input1 {
        case '[' :: '[' :: input2 => parseKey(input2, Nil) |> flatMap(match
            (input3, keys) -> 
            match input3 {
                case ']' :: ']' :: input4 => 
                    trimNewLine(input4) |> flatMap(
                    input5 -> 
                    parseTableLoop(input5, ArrayOfTable(keys) :: acc)
                    )
                case _ => Err("Expected ]], found something else")
            })
        case '[' :: input2 => parseKey(input2, Nil) |> flatMap(match
            (input3, keys) -> 
            match input3 {
                case ']' :: input4 => trimNewLine(input4) |> flatMap(
                    input5 ->
                    parseTableLoop(input5, Table(keys) :: acc1)
                    )
                case _ => Err("Expected ], found something else")
            })
        case Nil => Ok(acc)
        case _ => Err("Internal parser error")
    })

def parseInternalLoop(
    input: List[Char], acc: List[TomlLine]
): Result[(List[Char], List[TomlLine]), String] =
    use Result.flatMap;
    match input {
        case Nil => Ok(input, acc)
        case _ => input.parseKey(Nil) |> flatMap(match
            (input1, keys) ->
            match input1.skipSpace() {
                case '=' :: input2 => input2.skipSpace().takeValue() |> flatMap(match
                    (input3, value) ->
                    input3.trimNewLine() |> flatMap(
                    input4 ->
                    parseInternalLoop(input4, KeyValuePair((keys, value)) :: acc)
                    ))
                case _ => Err("Expected '=' after key, found something else")
            })
    }

def parseKey(
    input: List[Char],
    acc: List[String]
): Result[(List[Char], List[String]), String] =
    use Result.flatMap;
    match input {
        case '"' :: input1 => parseQuotedKey(input1, Nil) |> flatMap(match
            (input2, key) -> afterKey(input2, key :: acc)
        )
        case '\'' :: input1 => parseLiteralKey(input1, Nil) |> flatMap(match
            (input2, key) -> afterKey(input2, key :: acc)
        )
        case Nil => Err("Expected key, found empty input")
        case input1 => parseBareKey(input1, Nil) |> flatMap(match
            (input2, key) -> afterKey(input2, key :: acc)
        )
    }

def afterKey(
    input: List[Char],
    acc: List[String]
): Result[(List[Char], List[String]), String] = match input.skipSpace() {
    case '.' :: input1 => input1.skipSpace().parseKey(acc)
    case input1 => Ok((input1, List.reverse(acc)))
}

def parseBareKey(
    input: List[Char],
    acc: List[Char]
): Result[(List[Char], String), String] = match input {
    case x :: input1 => if (isBareKeyCharacter(x)) {
        parseBareKey(input1, x :: acc)
    } else if (List.isEmpty(acc)) {
        Err("Encountered empty key")
    } else {
        Ok(input, List.toString(List.reverse(acc)))
    }
    case Nil => Err("Unreachable!")
}

def parseQuotedKey(
    input: List[Char],
    acc: List[Char]
): Result[(List[Char], String), String] = ???

def parseLiteralKey(
    input: List[Char],
    acc: List[Char]
): Result[(List[Char], String), String] = ???

def takeValue(
    input: List[Char]
): Result[(List[Char], String), String] =
    use Result.flatMap;
    match input {
        case '"' :: '"' :: '"' :: input1 => parseMultilineBasicString(input1, Nil)
        case '"' :: input1 => parseBasicString(input1, Nil)
        case '\'' :: '\'' :: '\'' :: input1 => parseMultilineLiteralString(input1, Nil)
        case '\'' :: input1 => parseLiteralString(input1, Nil)
        case '[' :: input1 => parseArray(input1) |> flatMap(match
            (input2, value) ->
            match input2 {
                case ']' :: input3 => Ok((input3, value))
                case _ => Err("Expected closing bracket on array, found something else")
            }
        )
        case '{' :: input1 => parseInlineTable(input1, Nil) |> flatMap(match
            (input2, value) ->
            match input2 {
                case '}' :: input3 => Ok((input3, value))
                case _ => Err("Expected closing brace on inline table, found something else")
            }
        )
        case x :: _ => if (x == '-' || x == '+' || Char.isAsciiDigit(x) || x == 'i' || x == 'n') {
            parseNumberOrDate(input, Nil)
        } else if (x == 't' || x == 'f') {
            parseBool(input)
        } else {
            Err("Expected value, but found nothing recogniseable")
        }
        case Nil => Err("Expected value, but found end of input")
    }

// Possibly these should return some kind of enum descriping what they are, and their value, or a
// placeholder string until it makes sense to parse it. A problem for later me.
def parseNumberOrDate(
    input: List[Char],
    acc: List[Char]
): Result[(List[Char], String), String] = ???

def parseInlineTable(
    input: List[Char],
    acc: List[Char]
): Result[(List[Char], String), String] = ???

def parseArray(
    input: List[Char]
): Result[(List[Char], String), String] = ???

def parseLiteralString(
    input: List[Char],
    acc: List[Char]
): Result[(List[Char], String), String] = ???

def parseMultilineLiteralString(
    input: List[Char],
    acc: List[Char]
): Result[(List[Char], String), String] = ???

def parseBasicString(
    input: List[Char],
    acc: List[Char]
): Result[(List[Char], String), String] = ???

def parseMultilineBasicString(
    input: List[Char],
    acc: List[Char]
): Result[(List[Char], String), String] = ???

def parseBool(
    input: List[Char]
): Result[(List[Char], String), String] = ???

// Trims at least one new line. Returns error if a non-whitespace character was encountered before
// the newline, or if illegal character sequences are encountered. Returns Nil if end of character
// stream is reached.
def trimNewLine(input: List[Char]): Result[List[Char], String] = ???

def skipSpace(input: List[Char]): List[Char] = ???

def isBareKeyCharacter(x: Char): Bool =
    (Char.isAscii(x) && Char.isLetter(x)) || Char.isAsciiDigit(x) || x == '_' || x == '-'
