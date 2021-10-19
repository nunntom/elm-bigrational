module Arithmetic exposing (arithmetic, safeFloat)

import BigInt
import BigRational as BR exposing (BigRational)
import Expect
import Fuzz exposing (float, floatRange)
import Test exposing (..)


arithmetic : Test
arithmetic =
    describe "Arithmetic and Comparing"
        [ describe "Math"
            [ fuzzMath BR.add (+) "Add and floor"
            , fuzzMath BR.sub (-) "Sub and floor"
            , fuzz (Fuzz.tuple ( float, float )) "Div and Mul" <|
                \( f1, f2 ) ->
                    let
                        r1 =
                            BR.fromFloat f1

                        r2 =
                            BR.fromFloat f2
                    in
                    Expect.equal (BR.div r1 r2)
                        (BR.mul r1 (BR.div (BR.fromInt 1) r2))
            ]
        ]


fuzzMath : (BigRational -> BigRational -> BigRational) -> (Float -> Float -> Float) -> String -> Test
fuzzMath ratioMath basicMath name =
    fuzz (Fuzz.tuple ( safeFloat, safeFloat )) name <|
        \( f1, f2 ) ->
            let
                r1 =
                    BR.fromFloat f1

                r2 =
                    BR.fromFloat f2
            in
            Expect.equal (ratioMath r1 r2 |> BR.floor) (Basics.floor (basicMath f1 f2) |> BigInt.fromInt)


safeFloat : Fuzz.Fuzzer Float
safeFloat =
    floatRange -2147483648 2147483647
