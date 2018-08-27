open Json_decoder

let check ?(label="decoder") checker got expected () =
  Alcotest.(check @@ result checker string )
    label
    got
    expected

let simple_decoders =
  let checker = check ~label:"primitive decoder" in
  [ "int", `Quick,
    checker Alcotest.int
      Decoder.(decode_string int "23")
      ( Result.Ok 23 )
  ; "int", `Quick,
    checker Alcotest.int
      Decoder.(decode_string int "46")
      ( Result.Ok 46 )
  ; "float", `Quick,
    checker (Alcotest.float 1.)
      Decoder.(decode_string float "2.3")
      ( Result.Ok 2.3 )
  ; "string", `Quick,
    checker Alcotest.string
      Decoder.(decode_string string "\"twenty-three\"")
      ( Result.Ok "twenty-three" )
  ; "bool-true", `Quick,
    checker Alcotest.bool
      Decoder.(decode_string bool "true")
      ( Result.Ok true )
  ; "bool-false", `Quick,
    checker Alcotest.bool
      Decoder.(decode_string bool "false")
      ( Result.Ok false )
  ; "list-int-index", `Quick,
    checker Alcotest.int
      Decoder.(decode_string (index 1 int) "[1,48,3]")
      ( Result.Ok 48 )
  ; "dict-field", `Quick,
    checker (Alcotest.float 1.)
      Decoder.(decode_string (field "lat" float) "{\"lat\": 52.3}")
      ( Result.Ok 52.3 )
  ; "dict-field", `Quick,
    checker (Alcotest.float 1.)
      Decoder.(decode_string (field "lng" float) "{\"lat\": 52.3}")
      ( Result.Error "key lng does not exist in object lat " )
  ; "list", `Quick,
    checker Alcotest.(list int)
      Decoder.(decode_string (list int) "[1,48,3]")
      ( Result.Ok [1;48;3] )
  ; "array", `Quick,
    checker Alcotest.(array int)
      Decoder.(decode_string (array int) "[1,48,3]")
      ( Result.Ok [|1;48;3|] )
  ; "pairs", `Quick,
    checker Alcotest.(list (pair string int))
      Decoder.(decode_string (pairs int) "{\"lat\": 5, \"lng\": 15}")
      ( Result.Ok ["lat",5; "lng",15] )
  ; "mapN", `Quick,
    checker Alcotest.string
      Decoder.(decode_string (
          mapN (Printf.sprintf "{lat:%d, lng:%d}")
          ||> "lat" @= int
          ||> "lng" @= int
        )
          "{\"lat\": 5, \"lng\": 15}"
        )
      ( Result.Ok "{lat:5, lng:15}" )
  ]


let () =
    Alcotest.run "test suite"
    [ "primitives", simple_decoders
    ]
