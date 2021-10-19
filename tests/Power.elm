module Power exposing (power)

import BigRational as BR
import Expect
import Fuzz exposing (float)
import Test exposing (..)


power : Test
power =
    describe "To the power"
        [ fuzz (Fuzz.tuple ( float, Fuzz.intRange -10 10 ))
            "From float and to the power of an int"
          <|
            \( f, i ) ->
                BR.fromFloat f
                    |> BR.pow i
                    |> BR.toFloat
                    |> Expect.within (Expect.Relative 1.0e-8) (f ^ Basics.toFloat i)
        ]
