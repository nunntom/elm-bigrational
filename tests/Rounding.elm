module Rounding exposing (..)

import Arithmetic exposing (safeFloat)
import BigInt exposing (BigInt)
import BigRational as BR exposing (BigRational)
import Expect
import Fuzz exposing (int)
import Test exposing (..)


rounding : Test
rounding =
    describe "Rounding tests"
        [ fuzzRounding (BR.round BR.ToInfinity) Basics.round "Fuzz from float and round"
        , fuzzRounding BR.floor Basics.floor "Fuzz from float and floor"
        , fuzzRounding BR.ceiling Basics.ceiling "Fuzz from float and ceiling"
        , fuzzRounding BR.truncate Basics.truncate "Fuzz from float and truncate"
        , describe "Midpoint rounding"
            [ fuzz int "Midpoint to infinity rounding" <|
                roundHelper BR.ToInfinity toInfinity
            , fuzz int "Midpoint away from zero rounding" <|
                roundHelper BR.AwayFromZero awayFromZero
            , fuzz int "Midpoint to even rounding" <|
                roundHelper BR.ToEven toEven
            , fuzz int "Midpoint to odd rounding" <|
                roundHelper BR.ToOdd toOdd
            ]
        ]


fuzzRounding : (BigRational -> BigInt) -> (Float -> Int) -> String -> Test
fuzzRounding ratioRound basicsRound name =
    fuzz safeFloat name <|
        \fuzzFloat ->
            let
                bigInt =
                    BR.fromFloat fuzzFloat
                        |> ratioRound
            in
            Expect.equal bigInt (BigInt.fromInt <| basicsRound fuzzFloat)


roundHelper : BR.MidpointRounding -> (Int -> Int) -> Int -> Expect.Expectation
roundHelper mr toTargetInt sourceInt =
    Expect.equal
        ((String.fromInt sourceInt
            ++ ".5"
         )
            |> BR.fromFloatString
            |> Maybe.map (BR.round mr)
        )
        (toTargetInt sourceInt |> BigInt.fromInt |> Just)


toEven : Int -> Int
toEven i =
    if Basics.modBy 2 i == 0 then
        i

    else if i < 0 then
        i - 1

    else
        i + 1


toOdd : Int -> Int
toOdd i =
    if Basics.modBy 2 i == 1 then
        i

    else if i < 0 then
        i - 1

    else
        i + 1


toInfinity : Int -> Int
toInfinity i =
    if i < 0 then
        i

    else
        i + 1


awayFromZero : Int -> Int
awayFromZero i =
    if i < 0 then
        i - 1

    else
        i + 1
