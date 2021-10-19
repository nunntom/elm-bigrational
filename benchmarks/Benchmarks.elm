module Benchmarks exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BigRational as BR exposing (..)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        piFloat =
            22 / 7

        piRational =
            fromInts 22 7

        ratFloat =
            100 / 3

        rat33 =
            fromInts 100 3
    in
    describe "BigRational Benchmarks"
        [ benchmark "From float to rational"
            (\_ -> fromFloat piFloat)
        , benchmark "To float"
            (\_ -> piRational |> BR.toFloat)
        , Benchmark.compare "Divide"
            "Basics"
            (\_ -> piFloat / ratFloat)
            "BigRational"
            (\_ -> div piRational rat33)
        , Benchmark.compare "To power of 10"
            "Basics"
            (\_ -> piFloat ^ 10)
            "BigRational"
            (\_ -> pow 10 piRational)
        ]
