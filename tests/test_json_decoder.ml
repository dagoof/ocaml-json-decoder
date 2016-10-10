open Json_decoder

let simple_decoders =
    let check checker got expected () =
        Alcotest.(check @@ result checker string )
        "primitive decoder"
        got
        expected
    in
    [ "int", `Quick,
        check Alcotest.int
        Decoder.(decode_string int "23")
        ( Result.Ok 23 )
    ; "int", `Quick,
        check Alcotest.int
        Decoder.(decode_string int "46")
        ( Result.Ok 46 )
    ; "float", `Quick,
        check Alcotest.float
        Decoder.(decode_string float "2.3")
        ( Result.Ok 2.3 )
    ; "string", `Quick,
        check Alcotest.string
        Decoder.(decode_string string "\"twenty-three\"")
        ( Result.Ok "twenty-three" )
    ; "bool-true", `Quick,
        check Alcotest.bool
        Decoder.(decode_string bool "true")
        ( Result.Ok true )
    ; "bool-false", `Quick,
        check Alcotest.bool
        Decoder.(decode_string bool "false")
        ( Result.Ok false )
    ]


let () =
    Alcotest.run "test suite"
    [ "primitives", simple_decoders
    ]
