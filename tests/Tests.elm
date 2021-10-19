module Tests exposing (suite)

import BigRational as BR
import Expect
import Fuzz exposing (float, int)
import Test exposing (..)


suite : Test
suite =
    describe "Testing Ratios"
        [ describe "Build Rational"
            [ fuzz (Fuzz.tuple3 ( int, int, int )) "Make a ratio from 3 tuples and compare same ratio from string" <|
                \( i, n, d ) ->
                    let
                        ratio =
                            BR.add (BR.fromInt i) (BR.fromInts n d)

                        stringRatio =
                            String.fromInt i
                                ++ " "
                                ++ String.fromInt n
                                ++ "/"
                                ++ String.fromInt d
                                |> BR.fromString
                    in
                    Expect.equal (Just ratio) stringRatio
            , fuzz float
                "Make a ratio from a float then turn it back into a float string"
              <|
                \f ->
                    BR.fromFloat f
                        |> BR.toFloat
                        |> Expect.within (Expect.Absolute 0.000001) f
            ]
        , describe "Comparison"
            [ fuzz (Fuzz.tuple ( float, float )) "Compare" <|
                \( f1, f2 ) ->
                    Expect.equal (BR.compare (BR.fromFloat f1) (BR.fromFloat f2)) (Basics.compare f1 f2)
            ]
        ]
