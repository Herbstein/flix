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
}
