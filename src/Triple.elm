module Triple exposing (first)


first : ( a, b, c ) -> a
first ( a, _, _ ) =
    a
