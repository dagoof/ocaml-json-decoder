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
        check (Alcotest.float 1.)
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
    ; "list-int-index", `Quick,
        check Alcotest.int
        Decoder.(decode_string (index 1 int) "[1,48,3]")
        ( Result.Ok 48 )
    ; "dict-field", `Quick,
        check (Alcotest.float 1.)
        Decoder.(decode_string (field "lat" float) "{\"lat\": 52.3}")
        ( Result.Ok 52.3 )
    ; "dict-field", `Quick,
        check (Alcotest.float 1.)
        Decoder.(decode_string (field "lng" float) "{\"lat\": 52.3}")
        ( Result.Error "key lng does not exist in object lat " )
    ; "list", `Quick,
        check Alcotest.(list int)
        Decoder.(decode_string (list int) "[1,48,3]")
        ( Result.Ok [1;48;3] )
    ; "array", `Quick,
        check Alcotest.(array int)
        Decoder.(decode_string (array int) "[1,48,3]")
        ( Result.Ok [|1;48;3|] )
    ; "pairs", `Quick,
        check Alcotest.(list (pair string int))
        Decoder.(decode_string (pairs int) "{\"lat\": 5, \"lng\": 15}")
        ( Result.Ok ["lat",5; "lng",15] )
    ]


let () =
    Alcotest.run "test suite"
    [ "primitives", simple_decoders
    ]
