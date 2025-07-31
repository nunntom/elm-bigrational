module Power exposing (power)

import BigRational as BR
import Expect
import Fuzz exposing (float)
import Test exposing (..)


power : Test
power =
    describe "To the power"
        [ fuzz (Fuzz.pair (Fuzz.filter (\f -> not (isNaN f)) float) (Fuzz.intRange -10 10))
            "From float and to the power of an int"
          <|
            \( f, i ) ->
                BR.fromFloat f
                    |> BR.pow i
                    |> BR.toFloat
                    |> Expect.within (Expect.AbsoluteOrRelative 1.0e-8 1.0e-8) (f ^ Basics.toFloat i)
        , test "From float and to the power of an int (edge case)" <|
            \_ ->
                let
                    ( f, i ) =
                        ( 3.8893845486632136e-62, -4 )
                in
                BR.fromFloat f
                    |> BR.pow i
                    |> BR.toFloat
                    |> Expect.within (Expect.AbsoluteOrRelative 1.0e-8 1.0e-8) (f ^ Basics.toFloat i)
        ]
