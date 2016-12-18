///
/// The Option type.
///
/// An option represents an optional value.
/// The constructor `None` represents an absent value,
/// whereas the constructor `Some(v)` represents the value `v`.
///
enum Option[t] {
    case None,
    case Some(t)
}

///
/// The Result type.
///
/// A result represents a successful value or an error value.
/// The constructor `Ok(v)` represents the successful value `v`,
/// whereas the constructor `Err(v)` represents the error value `v`.
///
enum Result[t, e] {
    case Ok(t),
    case Err(e)
}

///
/// The List type.
///
/// A list is either the empty list represented by `Nil`, or
/// an element `v` followed by a list `vs` represented by `v :: vs`.
///
enum List[t] {
    case Nil,
    case Cons(t, List[t])
}

///
/// Alias for logical conjunction.
///
def ∧(x: Bool, y: Bool): Bool = x && y

///
/// Alias for logical disjunction.
///
def ∨(x: Bool, y: Bool): Bool = x || y

///
/// Alias for logical implication.
///
def →(x: Bool, y: Bool): Bool = !x ∨ y

///
/// Alias for logical biconditional.
///
def ↔(x: Bool, y: Bool): Bool = (x → y) ∧ (y → x)

///
/// Returns a freshly generated integer. (NB: Requires the --Ximpure feature flag.)
///
def genSym!: Int = genSymHook()

///
/// Prints the given value `a`. (NB: Requires the --Ximpure feature flag.)
///
def print![a](a: a): a = printHook(a)

///
/// Prints the given value `a` on a new line. (NB: Requires the --Ximpure feature flag.)
///
def println![a](a: a): a = printlnHook(a)