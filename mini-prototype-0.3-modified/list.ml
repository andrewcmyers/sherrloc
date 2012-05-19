let rec len l =
    match exp with
        Nil => 0
    |   Cons x xs => 1 + len xs
    end
