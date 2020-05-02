namespace Parser {
    type alias Parser[a] = List[Char] -> Option[(List[Char], a)]
    
    pub def char(): Parser[Char] = input -> match input {
        case Nil => None
        case x :: xs => Some((xs, x))
    }

    pub def filter[a](
        p: Parser[a],
        f: a -> Bool
    ): Parser[a] = input -> match input.p() {
        case Some((xs, v)) => if (v.f()) {
            Some((xs, v))
        } else {
            None
        }
        case None => None
    }

    pub def map[a, b](
        p: Parser[a],
        f: a -> b
    ): Parser[b] = input -> match input.p() {
        case Some((xs, v)) => Some((xs, v.f()))
        case None => None
    }
    pub def takeWhile[a](
        p: Parser[a]
    ): Parser[List[a]] = input -> Some(input.takeWhileInternal(p))

    def takeWhileInternal[a](
        input: List[Char],
        p: Parser[a]
    ): (List[Char], List[a]) = match input.p() {
        case Some((xs, v)) => {
            let (ys, u) = xs.takeWhileInternal(p);
            (ys, v :: u)
        }
        case None => (input, Nil)
    }

    pub def fold[a, b](
        p: Parser[a],
        initial: b,
        f: b -> a -> b
    ): Parser[b] = input -> Some(foldInternal(p, input, initial, f))

    /*pub def parse(input: List[Char]): Option[ParsedToml] = {
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
    }*/

    def foldInternal[a, b](
        p: Parser[a],
        input: List[Char],
        initial: b,
        f: b -> a -> b
        // (List[_], Map[k, v]) -> a -> (List[_], Map[k, v])
    ): (List[Char], b) = match input.p() {
        case Some((xs, v)) => foldInternal(p, xs, f(initial, v), f)
        case None => (input, initial)
    }

    pub def and[a, b](
        lhs: Parser[a],
        rhs: Parser[b]
    ): Parser[(a, b)] = input -> match input.lhs() {
        case Some((xs, v)) => match xs.rhs() {
            case Some((ys, u)) => Some((ys, (v, u)))
            case None => None
        }
        case None => None
    }

    pub def then[a, b](
        lhs: Parser[a],
        rhs: a -> Parser[b]
    ): Parser[b] = input -> match input.lhs() {
        case Some((xs, v)) => {
            let res = rhs(v);
            res(xs)
        }
        case None => None
    }

    pub def or[a](
        lhs: Parser[a],
        rhs: Parser[a]
    ): Parser[a] = input -> match input.lhs() {
        case None => input.rhs()
        case ret => ret
    }

    pub def delimit[a, b, c](
        lhs: Parser[a],
        mid: Parser[b],
        rhs: Parser[c]
    ): Parser[b] = lhs.then(_ -> mid.then(b -> rhs.map(_ -> b)))

    pub def tag(c: Char): Parser[Char] = char().filter(d -> d == c)

    pub def not[a](p: Parser[a]): Parser[()] = input -> match input.p() {
        case Some(_) => None
        case None => Some((input, ()))
    }

}
