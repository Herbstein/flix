/// Flix supports function composition with
/// the |> operator (among others) and currying.
/// This makes it easy to construct pipelines:

/// Constructs a list with ten elements and performs
/// various operations on it in a pipeline.
def main(): Bool =
    List.range(0, 10) |>
    List.map(x -> x * x) |>
    List.take(5) |>
    List.exists(x -> x == 1)
