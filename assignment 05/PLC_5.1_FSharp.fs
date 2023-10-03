let rec merge x : int list =
    match x with
    | [],[] -> []
    | x :: xs, [] -> x :: merge (xs, [])
    | [], y :: ys -> y :: merge ([], ys)
    | x :: xs, y :: ys when x > y -> y :: merge (x :: xs, ys)
    | x :: xs, y :: ys when x <= y -> x :: merge (xs, y :: ys);;