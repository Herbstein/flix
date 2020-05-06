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
    // takeAny                                                                                   //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def takeAny1(): Bool =
        use Parser.takeAny;

        match Nil |> takeAny(
            input -> match input {
                case 'a' :: xs => Some((xs, 'a'))
                case _ => None
            }
        ){
            case Some((Nil, Nil)) => true
            case _ => false
        }
    
    @test
    def takeAny2(): Bool =
        use Parser.takeAny;

        match ('a' :: Nil) |> takeAny(
            input -> match input {
                case 'a' :: xs => Some((xs, 'a'))
                case _ => None
            }
        ){
            case Some((Nil, 'a' :: Nil)) => true
            case _ => false
        }

    @test
    def takeAny3(): Bool = 
        use Parser.takeAny;

        match ('a' :: 'b' :: Nil) |> takeAny(
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
    def takeAny4(): Bool = 
        use Parser.takeAny;

        match ('a' :: 'b' :: '-' :: Nil) |> takeAny(
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
    def takeAny5(): Bool = 
        use Parser.takeAny;

        match ('a' :: 'b' :: '-' :: 'c' :: Nil) |> takeAny(
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
    // takeMany                                                                                  //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def takeMany1(): Bool = 
        use Parser.takeMany;

        match ('a' :: 'b' :: Nil) |> takeMany(
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
    def takeMany2(): Bool = 
        use Parser.takeMany;

        match ('a' :: 'b' :: '-' :: Nil) |> takeMany(
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
    def takeMany3(): Bool = 
        use Parser.takeMany;

        match ('a' :: 'b' :: '-' :: 'c' :: Nil) |> takeMany(
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

    @test
    def takeMany4(): Bool = 
        use Parser.takeMany;

        match ('_' :: 'a' :: 'b' :: 'c' :: Nil) |> takeMany(
            input -> match input {
                case x :: xs => if (Char.isLetter(x)) {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            }
        ){
            case None => true
            case _ => false
        }

    @test
    def takeMany5(): Bool = 
        use Parser.takeMany;

        match Nil |> takeMany(
            input -> match input {
                case x :: xs => if (Char.isLetter(x)) {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            }
        ){
            case None => true
            case _ => false
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////
    // tailSeperatedAny                                                                          //
    ///////////////////////////////////////////////////////////////////////////////////////////////

    @test
    def tailSeperatedAny1(): Bool = 
        use Parser.tailSeperatedAny;

        match Nil |> tailSeperatedAny(
            input -> match input {
                case x :: xs => if (Char.isLetter(x)) {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            },
            input -> match input {
                case x :: xs => if (x == '_') {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            }
        ){
            case Some((Nil, Nil)) => true
            case _ => false
        }

    @test
    def tailSeperatedAny2(): Bool = 
        use Parser.tailSeperatedAny;

        match ('a' :: 'b' :: Nil) |> tailSeperatedAny(
            input -> match input {
                case x :: xs => if (Char.isLetter(x)) {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            },
            input -> match input {
                case x :: xs => if (x == '_') {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            }
        ){
            case Some(('b' :: Nil, 'a' :: Nil)) => true
            case _ => false
        }

    @test
    def tailSeperatedAny3(): Bool = 
        use Parser.tailSeperatedAny;

        match ('a' :: '_' :: 'b' :: Nil) |> tailSeperatedAny(
            input -> match input {
                case x :: xs => if (Char.isLetter(x)) {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            },
            input -> match input {
                case x :: xs => if (x == '_') {
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
    def tailSeperatedAny4(): Bool = 
        use Parser.tailSeperatedAny;

        match ('a' :: '_' :: Nil) |> tailSeperatedAny(
            input -> match input {
                case x :: xs => if (Char.isLetter(x)) {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            },
            input -> match input {
                case x :: xs => if (x == '_') {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            }
        ){
            case Some((Nil, 'a' :: Nil)) => true
            case _ => false
        }

    @test
    def tailSeperatedAny5(): Bool = 
        use Parser.tailSeperatedAny;

        match ('a' :: '_' :: 'b' :: '_' :: '_' :: Nil) |> tailSeperatedAny(
            input -> match input {
                case x :: xs => if (Char.isLetter(x)) {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            },
            input -> match input {
                case x :: xs => if (x == '_') {
                    Some((xs, x))
                } else {
                    None
                }
                case _ => None
            }
        ){
            case Some(('_' :: Nil, 'a' :: 'b' :: Nil)) => true
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
