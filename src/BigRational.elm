module BigRational exposing
    ( BigRational
    , fromInt, fromBigInt, fromInts, fromBigInts, fromFloat, fromString, fromFloatString
    , toBigInts, toMixedFraction, toMixedParts, toDecimalString, toFloat, toString
    , add, sub, mul, div, pow, invert, negate, abs
    , compare, lt, gt
    , MidpointRounding(..), round, floor, ceiling, truncate
    )

{-| Big rational numbers. Represent and perform arithmetic on non-integer values
without floating point errors or native integer bounds.


# Definition

@docs BigRational


# From

@docs fromInt, fromBigInt, fromInts, fromBigInts, fromFloat, fromString, fromFloatString


# To

@docs toBigInts, toMixedFraction, toMixedParts, toDecimalString, toFloat, toString


# Arithmetic

@docs add, sub, mul, div, pow, invert, negate, abs


# Compare

@docs compare, lt, gt


# Rounding

@docs MidpointRounding, round, floor, ceiling, truncate

-}

import BigInt exposing (BigInt)
import Regex


{-| The BigRational opaque type.
-}
type BigRational
    = BigRational BigInt BigInt


{-| Make a rational from an integer.
-}
fromInt : Int -> BigRational
fromInt n =
    fromBigInts (BigInt.fromInt n) oneInt


{-| Make a rational from a BigInt.
-}
fromBigInt : BigInt -> BigRational
fromBigInt n =
    fromBigInts n oneInt


{-| Make a rational as the ratio of two integers. `fromInts 1 2` is a half (1/2).
-}
fromInts : Int -> Int -> BigRational
fromInts n d =
    fromBigInts (BigInt.fromInt n) (BigInt.fromInt d)


{-| Make a rational as the ratio of two BigInts. `fromBigInts 3 4` is 4 quarters.
-}
fromBigInts : BigInt -> BigInt -> BigRational
fromBigInts n d =
    let
        gcd_ =
            gcd n d

        a =
            BigInt.div n gcd_

        b =
            BigInt.div d gcd_

        ( a2, b2 ) =
            if BigInt.lt b zeroInt then
                ( BigInt.negate a, BigInt.negate b )

            else
                ( a, b )
    in
    BigRational a2 b2


{-| Make a rational out of a float.

    fromFloat 1.5 == fromInts 3 2

-}
fromFloat : Float -> BigRational
fromFloat f =
    String.fromFloat f
        |> fromFloatString
        |> Maybe.withDefault zero


{-| Try to make a rational from a string. The string can in the format
`"1/3"` or `"5 3/4"` or `"-4 3/25"` with a single space between
the integer part (if present) and the fraction.

    fromString "5 10/100" == Just (fromFloat 5.1)

    fromString "12/5" == Just (fromFloat 2.4)

    fromString "-1/3" == Just (fromInts -1 3)

-}
fromString : String -> Maybe BigRational
fromString s =
    let
        parts =
            String.split " " s
                |> List.map (String.split "/")
                |> List.concat
                |> List.map BigInt.fromIntString
    in
    case parts of
        [ Just i ] ->
            Just (fromBigInt i)

        [ Just n, Just d ] ->
            Just (fromBigInts n d)

        [ Just i, Just n, Just d ] ->
            Just <| add (fromBigInt i) (fromBigInts n d)

        _ ->
            Nothing


{-| Try to make a rational from a string representation of a float/decimal number.

    fromFloatString "1.5" == Just (fromInts 3 2)

    fromFloatString "5e-2" == fromString "100/5"

    fromFloatString "1.5E7" == fromString "15000000"

-}
fromFloatString : String -> Maybe BigRational
fromFloatString s =
    if s == "Infinity" then
        Just (fromInts 1 0)

    else if s == "NaN" then
        Just (fromInts 0 0)

    else
        let
            makeRegex regex =
                Maybe.withDefault Regex.never <|
                    Regex.fromString regex
        in
        case
            Regex.splitAtMost 1 (makeRegex "[eE]") s
        of
            [ dec, exp ] ->
                fromFloatString dec
                    |> Maybe.map2 mul (Maybe.map (\e -> pow e (fromInt 10)) (String.toInt exp))

            [ dec ] ->
                case
                    ( String.replace "." "" dec
                        |> trimLeftZeros
                        |> BigInt.fromIntString
                    , Regex.splitAtMost 1 (makeRegex "\\.") dec
                    )
                of
                    ( Just int, [ _ ] ) ->
                        Just (fromBigInts int oneInt)

                    ( Just int, [ _, decs ] ) ->
                        let
                            p =
                                BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt <| String.length decs)
                        in
                        Just (fromBigInts int p)

                    _ ->
                        Nothing

            _ ->
                Nothing


{-| Add two big rationals.

    add (fromInts 1 2) (fromInts 1 4) == fromInts 3 4

-}
add : BigRational -> BigRational -> BigRational
add (BigRational n1 d1) (BigRational n2 d2) =
    fromBigInts (BigInt.add (BigInt.mul n1 d2) (BigInt.mul n2 d1)) (BigInt.mul d1 d2)


{-| Subtract one big rational from another.

    sub (fromInt 10) (fromFloat 2.5) == fromInts 15 2

-}
sub : BigRational -> BigRational -> BigRational
sub (BigRational n1 d1) (BigRational n2 d2) =
    fromBigInts (BigInt.sub (BigInt.mul n1 d2) (BigInt.mul n2 d1)) (BigInt.mul d1 d2)


{-| Multiply two big rationals.

    mul (fromInts 5 2) (fromInts 3 2) == fromInts 15 4

-}
mul : BigRational -> BigRational -> BigRational
mul (BigRational n1 d1) (BigRational n2 d2) =
    fromBigInts (BigInt.mul n1 n2) (BigInt.mul d1 d2)


{-| Divide one big rational by another.
-}
div : BigRational -> BigRational -> BigRational
div (BigRational n1 d1) (BigRational n2 d2) =
    fromBigInts (BigInt.mul n1 d2) (BigInt.mul d1 n2)


{-| Raise a big rational to the power of an integer.

    (fromInts 5 2 |> pow 2 |> toString) == "6 1/4"

    (fromInts 5 2 |> pow -2 |> toFloat) == 0.16

-}
pow : Int -> BigRational -> BigRational
pow p x =
    if p == 0 then
        fromInt 1

    else if x == zero then
        if p < 0 then
            BigRational oneInt zeroInt

        else
            zero

    else
        let
            (BigRational n d) =
                x

            p2 =
                Basics.abs p
                    |> BigInt.fromInt

            n2 =
                BigInt.pow n p2

            d2 =
                BigInt.pow d p2
        in
        if p < 0 then
            fromBigInts d2 n2

        else
            fromBigInts n2 d2


{-| Invert a rational. Flip the fraction upside down. 1/2 becomes 2/1.

    BR.invert (BR.fromInts 1 2) == BR.fromInts 2 1

-}
invert : BigRational -> BigRational
invert (BigRational n d) =
    BigRational d n


{-| Negate a rational.

    (fromInts 10 3 |> negate |> toString) == "-10/3"

-}
negate : BigRational -> BigRational
negate (BigRational n d) =
    BigRational (BigInt.negate n) d


{-| Get the [absolute](https://en.wikipedia.org/wiki/Absolute_value) value of a rational

    abs (fromInts -10 3) == fromInts 10 3

-}
abs : BigRational -> BigRational
abs n =
    if neg n then
        negate n

    else
        n


{-| Compare two rationals.

    compare (fromInts 3 2) (fromInts 1 2) == GT

-}
compare : BigRational -> BigRational -> Order
compare (BigRational n1 d1) (BigRational n2 d2) =
    BigInt.compare (BigInt.mul n1 d2) (BigInt.mul n2 d1)


{-| Is one rational greater than another?
-}
gt : BigRational -> BigRational -> Bool
gt a b =
    compare a b == GT


{-| Is one rational lesser than another?
-}
lt : BigRational -> BigRational -> Bool
lt a b =
    compare a b == LT


{-| Turn a rational into a [mixed fraction](https://www.mathsisfun.com/mixed-fractions.html). That is, a BigInt,
and a BigRational representing a [proper fraction](https://www.mathsisfun.com/proper-fractions.html).

    toMixedFraction (fromInts 25 2) == ( BigInt.fromInt 12, fromInts 1 2 )

-}
toMixedFraction : BigRational -> ( BigInt, BigRational )
toMixedFraction (BigRational n d) =
    ( BigInt.div n d
    , fromBigInts (remainder d n) d
    )


{-| When rounding, if the number is exactly half-way between two whole numbers, which way to go?
-}
type MidpointRounding
    = ToEven
    | ToOdd
    | ToInfinity
    | AwayFromZero


{-| Round a rational number to the nearest whole number.

    round ToEven (fromInts 10 3) == BigInt.fromInt 3

    round ToEven (fromFloat 1.5) == BigInt.fromInt 2

    round ToEven (fromFloat 2.5) == BigInt.fromInt 2

    round ToOdd (fromFloat 2.5) == BigInt.fromInt 3

    round ToInfinity (fromFloat 2.5) == BigInt.fromInt 3

    round AwayFromZero (fromFloat -2.5) == BigInt.fromInt -3

    round ToInfinity (fromFloat -2.5) == BigInt.fromInt -2

-}
round : MidpointRounding -> BigRational -> BigInt
round mr x =
    let
        ( int, rem ) =
            toMixedFraction x
    in
    if lt (abs rem) half then
        int

    else if mr == ToEven && abs rem == half && BigInt.isEven int then
        int

    else if mr == ToOdd && abs rem == half && BigInt.isOdd int then
        int

    else if neg x then
        if mr == ToInfinity && abs rem == half then
            int

        else
            BigInt.sub int oneInt

    else
        BigInt.add int oneInt


{-| Truncate a rational, rounding toward zero.
-}
truncate : BigRational -> BigInt
truncate (BigRational n d) =
    BigInt.div n d


{-| Floor means always rounding down, away from infinity.
-}
floor : BigRational -> BigInt
floor x =
    let
        ( int, rem ) =
            toMixedFraction x
    in
    if neg x && not (rem == zero) then
        BigInt.sub int oneInt

    else
        int


{-| The opposite of floor. Round up.
-}
ceiling : BigRational -> BigInt
ceiling x =
    let
        ( int, rem ) =
            toMixedFraction x
    in
    if rem == zero then
        int

    else if neg rem then
        int

    else
        BigInt.add int oneInt


{-| Get the numerator and denominator of the rational as two BigInts
-}
toBigInts : BigRational -> ( BigInt, BigInt )
toBigInts (BigRational n d) =
    ( n, d )


{-| Turn a rational into a [mixed fraction](https://www.mathsisfun.com/mixed-fractions.html) and get the individual parts as BigInts. This may be useful for displaying the number in your own way.
-}
toMixedParts : BigRational -> ( BigInt, BigInt, BigInt )
toMixedParts x =
    let
        ( int, BigRational n d ) =
            toMixedFraction x
    in
    ( int, n, d )


{-| Turn a rational into a float approximation.

    (fromInts 10 3 |> toFloat) == 3.3333333333333335

    (fromInts 1 2000000 |> toFloat) == 5.0e-7

-}
toFloat : BigRational -> Float
toFloat (BigRational n d) =
    let
        f1 =
            BigInt.toString n
                |> String.toFloat
                |> Maybe.withDefault 0

        f2 =
            BigInt.toString d
                |> String.toFloat
                |> Maybe.withDefault 0
    in
    f1 / f2


{-| Turn a rational into a decimal string with the specified number of decimal places.

    (fromInts 10 3 |> toDecimalString 5) == "3.33333"

    (fromInts 20 3 |> toDecimalString 10) == "6.6666666667"

    (fromInts 22 7 |> toDecimalString 30) == "3.142857142857142857142857142857"

-}
toDecimalString : Int -> BigRational -> String
toDecimalString places x =
    if isNaN x then
        "NaN"

    else if isInfinity x then
        "Infinity"

    else
        let
            i =
                abs x
                    |> mul
                        (fromBigInt <|
                            BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt places)
                        )
                    |> round ToEven
                    |> BigInt.toString

            int =
                String.dropRight places i

            dec =
                String.dropLeft (String.length int) i
                    |> String.padLeft places '0'
                    |> trimRightZeros
        in
        String.concat
            [ if lt x zero then
                "-"

              else
                ""
            , if String.isEmpty int then
                "0"

              else
                int
            , if String.isEmpty dec then
                ""

              else
                "." ++ dec
            ]


{-| Turn a rational into a string like `"1/2"` or `"5 3/4"` or `"-3 5/8"`.

    toString (fromFloat 2.25) == "2 1/4"

-}
toString : BigRational -> String
toString x =
    let
        ( int, rem ) =
            toMixedFraction x

        (BigRational n d) =
            rem
    in
    if rem == zero then
        BigInt.toString int

    else if int == zeroInt then
        BigInt.toString n ++ "/" ++ BigInt.toString d

    else
        BigInt.toString int
            ++ " "
            ++ BigInt.toString
                (if neg x then
                    BigInt.abs n

                 else
                    n
                )
            ++ "/"
            ++ BigInt.toString d



-- INTERNAL HELPERS


gcd : BigInt -> BigInt -> BigInt
gcd a b =
    if a == zeroInt then
        b

    else
        gcd (remainder a b)
            a


zeroInt : BigInt
zeroInt =
    BigInt.fromInt 0


oneInt : BigInt
oneInt =
    BigInt.fromInt 1


half : BigRational
half =
    fromBigInts (BigInt.fromInt 1) (BigInt.fromInt 2)


zero : BigRational
zero =
    fromInt 0


neg : BigRational -> Bool
neg x =
    lt x zero


remainder : BigInt -> BigInt -> BigInt
remainder a b =
    BigInt.modBy a b
        |> Maybe.withDefault zeroInt


trimRightZeros : String -> String
trimRightZeros s =
    Regex.fromString "0+$"
        |> Maybe.map (\r -> Regex.replace r (always "") s)
        |> Maybe.withDefault s


trimLeftZeros : String -> String
trimLeftZeros s =
    Regex.fromString "^0+"
        |> Maybe.map (\r -> Regex.replace r (always "") s)
        |> Maybe.withDefault s
        |> (\s2 ->
                if String.isEmpty s2 then
                    "0"

                else
                    s2
           )


isNaN : BigRational -> Bool
isNaN (BigRational n d) =
    n == zeroInt && d == zeroInt


isInfinity : BigRational -> Bool
isInfinity (BigRational _ d) =
    d == zeroInt
