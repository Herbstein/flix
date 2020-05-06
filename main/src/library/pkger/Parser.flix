namespace Parser {
    type alias Parser[a] = List[Char] -> Option[(List[Char], a)]
    
    pub def pure[a](
        ret: a
    ): Parser[a] = input -> Some((input, ret))

    pub def char(): Parser[Char] = input -> match input {
        case Nil => None
        case x :: xs => Some((xs, x: Char))
    }

    pub def tag(c: Char): Parser[Char] = char().filter(d -> d == c)

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

    pub def optional[a](
        p: Parser[a]
    ): Parser[Option[a]] = input -> match input.p() {
        case None => Some((input, None))
        case Some((xs, parsed)) => Some((xs, Some(parsed)))
    }

    pub def skip[a](
        p: Parser[a]
    ): Parser[Unit] = input -> match input.p() {
        case None => None
        case Some((xs, _)) => Some((xs, ()))
    }

    pub def skipLeft[a, b](
        lhs: Parser[a],
        rhs: Parser[b]
    ): Parser[b] = input -> match input.lhs() {
        case None => None
        case Some((xs, _)) => xs |> rhs
    }

    pub def skipRight[a, b](
        lhs: Parser[a],
        rhs: Parser[b]
    ): Parser[a] = input -> match input.lhs() {
        case None => None
        case Some((xs, parsed)) => xs |> rhs.map(_ -> parsed)
    }

    pub def takeAny[a](
        p: Parser[a]
    ): Parser[List[a]] = input -> Some(input.takeAnyInternal(p))

    def takeAnyInternal[a](
        input: List[Char],
        p: Parser[a]
    ): (List[Char], List[a]) = match input.p() {
        case Some((xs, v)) => {
            let (ys, u) = xs.takeAnyInternal(p);
            (ys, v :: u)
        }
        case None => (input, Nil)
    }

    pub def takeMany[a](
        p: Parser[a]
    ): Parser[List[a]] = input -> match takeAnyInternal(input, p) {
        case (_, Nil) => None
        case ret => Some(ret)
    }

    pub def seperatedMany[a, b](
        p: Parser[a],
        sep: Parser[b]
    ): Parser[List[a]] = (p `and` takeAny(sep `skipLeft` p)).map(match (lhs, rhs) -> lhs :: rhs)

    pub def seperatedAny[a, b](
        p: Parser[a],
        sep: Parser[b]
    ): Parser[List[a]] = seperatedMany(p, sep) `or` pure(Nil)

    pub def tailSeperatedMany[a, b](
        p: Parser[a],
        sep: Parser[b]
    ): Parser[List[a]] = seperatedMany(p, sep) `skipRight` optional(sep)

    pub def tailSeperatedAny[a, b](
        p: Parser[a],
        sep: Parser[b]
    ): Parser[List[a]] = tailSeperatedMany(p, sep) `or` pure(Nil)


    pub def fold[a, b](
        p: Parser[a],
        initial: b,
        f: b -> a -> b
    ): Parser[b] = input -> Some(foldInternal(p, input, initial, f))

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
    ): Parser[b] = lhs `skipLeft` mid `skipRight` rhs

    pub def not[a](p: Parser[a]): Parser[()] = input -> match input.p() {
        case Some(_) => None
        case None => Some((input, ()))
    }

}
