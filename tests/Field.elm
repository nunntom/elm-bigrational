module Field exposing (suite)

import BigInt exposing (BigInt)
import BigRational as BR exposing (BigRational)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (describe, fuzz, fuzz2, fuzz3)


suite : Test.Test
suite =
    describe "BigRational is a field"
        [ describe "BigRational is a commutative group with addition"
            [ fuzz3 fuzzRational fuzzRational fuzzRational "+ is associative" <|
                \a b c ->
                    BR.add (BR.add a b) c
                        |> expectEqual (BR.add a (BR.add b c))
            , fuzz fuzzRational "0 is a left identity for +" <|
                \a ->
                    BR.add zero a
                        |> expectEqual a
            , fuzz fuzzRational "Every element has an opposite" <|
                \a ->
                    BR.add a (BR.negate a)
                        |> expectEqual zero
            , fuzz2 fuzzRational fuzzRational "+ is commutative" <|
                \a b ->
                    BR.add a b
                        |> expectEqual (BR.add b a)
            ]
        , describe "BigRational is a commutative group with multiplication"
            [ fuzz3 fuzzRational fuzzRational fuzzRational "* is associative" <|
                \a b c ->
                    BR.mul (BR.mul a b) c
                        |> expectEqual (BR.mul a (BR.mul b c))
            , fuzz fuzzRational "1 is a left identity for *" <|
                \a ->
                    BR.mul one a
                        |> expectEqual a
            , fuzz fuzzNonzeroRational "Every nonzero element has an inverse" <|
                \a ->
                    BR.mul a (BR.invert a)
                        |> expectEqual one
            , fuzz2 fuzzRational fuzzRational "* is commutative" <|
                \a b ->
                    BR.mul a b
                        |> expectEqual (BR.mul b a)
            ]
        , fuzz3 fuzzRational fuzzRational fuzzRational "BigRational + and * distribute" <|
            \a b c ->
                BR.mul (BR.add a b) c
                    |> expectEqual (BR.add (BR.mul a c) (BR.mul b c))
        ]


expectEqual : BigRational -> BigRational -> Expect.Expectation
expectEqual expected actual =
    if expected == actual then
        Expect.pass

    else
        actual
            |> BR.toString
            |> Expect.equal (expected |> BR.toString)


zero : BigRational
zero =
    BR.fromInt 0


one : BigRational
one =
    BR.fromInt 1


fuzzRational : Fuzzer BigRational
fuzzRational =
    Fuzz.oneOf
        [ Fuzz.map BR.fromFloat Fuzz.niceFloat
        , Fuzz.map2 BR.fromBigInts fuzzBigInt fuzzNonzeroBigInt
        ]


fuzzBigInt : Fuzzer BigInt
fuzzBigInt =
    Fuzz.oneOf
        [ Fuzz.map BigInt.fromInt Fuzz.int
        , Fuzz.filterMap (\s -> BigInt.fromIntString ("+" ++ s)) fuzzIntString
        , Fuzz.filterMap (\s -> BigInt.fromIntString s) fuzzIntString
        , Fuzz.filterMap (\s -> BigInt.fromIntString ("-" ++ s)) fuzzIntString
        ]


fuzzNonzeroRational : Fuzzer BigRational
fuzzNonzeroRational =
    Fuzz.oneOf
        [ Fuzz.map BR.fromFloat (Fuzz.filter ((/=) 0) Fuzz.niceFloat)
        , Fuzz.map2 BR.fromBigInts fuzzNonzeroBigInt fuzzNonzeroBigInt
        ]


fuzzNonzeroBigInt : Fuzzer BigInt
fuzzNonzeroBigInt =
    fuzzBigInt
        |> Fuzz.filter (\b -> b /= bigIntZero)


bigIntZero : BigInt
bigIntZero =
    BigInt.fromInt 0


fuzzIntString : Fuzzer String
fuzzIntString =
    Fuzz.listOfLengthBetween 1 16 fuzzDigit
        |> Fuzz.map String.fromList


fuzzDigit : Fuzzer Char
fuzzDigit =
    List.range 0 9
        |> List.map (\v -> Char.fromCode (v + Char.toCode '0'))
        |> Fuzz.oneOfValues
