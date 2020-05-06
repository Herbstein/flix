namespace Parser/Test {

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // char                                                                                      //
    ///////////////////////////////////////////////////////////////////////////////////////////////
    @test
    def char1(): Bool =
        use Parser.char;
        match ('a' :: Nil) |> char() {
            case Some((Nil, 'a')) => true
            case _ => false
        }

    @test
    def char2(): Bool =
        use Parser.char;
        match (Nil) |> char() {
            case None => true
            case _ => false
        }

    @test
    def char3(): Bool =
        use Parser.char;
        match ('b' :: 'a' :: Nil) |> char() {
            case Some(('a' :: Nil, 'b')) => true
            case _ => false
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                                                 //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile1(): Bool =
        use Parser.takeWhile;

        match Nil |> takeWhile(
            input -> match input {
                case 'a' :: xs => Some((xs, 'a'))
                case _ => None
            }
        ){
            case Some((Nil, Nil)) => true
            case _ => false
        }
    
    @test
    def takeWhile2(): Bool =
        use Parser.takeWhile;

        match ('a' :: Nil) |> takeWhile(
            input -> match input {
                case 'a' :: xs => Some((xs, 'a'))
                case _ => None
            }
        ){
            case Some((Nil, 'a' :: Nil)) => true
            case _ => false
        }

    @test
    def takeWhile3(): Bool = 
        use Parser.takeWhile;

        match ('a' :: 'b' :: Nil) |> takeWhile(
            input -> match input {
                case x :: xs => if (Char.isLetter(x)) {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            }
        ){
            case Some((Nil, 'a' :: 'b' :: Nil)) => true
            case _ => false
        }

    @test
    def takeWhile4(): Bool = 
        use Parser.takeWhile;

        match ('a' :: 'b' :: '-' :: Nil) |> takeWhile(
            input -> match input {
                case x :: xs => if (Char.isLetter(x)) {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            }
        ){
            case Some(('-' :: Nil, 'a' :: 'b' :: Nil)) => true
            case _ => false
        }

    @test
    def takeWhile5(): Bool = 
        use Parser.takeWhile;

        match ('a' :: 'b' :: '-' :: 'c' :: Nil) |> takeWhile(
            input -> match input {
                case x :: xs => if (Char.isLetter(x)) {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            }
        ){
            case Some(('-' :: 'c' :: Nil, 'a' :: 'b' :: Nil)) => true
            case _ => false
        }

    
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // delimit                                                                                   //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def delimit1(): Bool =
        use Parser.delimit;
        match ('[' :: 'x' :: ']' :: Nil) |> delimit(
            input -> match input {
                case '[' :: xs => Some((xs, '['))
                case _ => None
            },
            input -> match input {
                case 'x' :: xs => Some((xs, 'x'))
                case _ => None
            },
            input -> match input {
                case ']' :: xs => Some((xs, ']'))
                case _ => None
            }
        ) {
            case Some((Nil, 'x')) => true
            case _ => false
        }
}
