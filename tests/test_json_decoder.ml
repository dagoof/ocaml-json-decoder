open Json_decoder

let check2 checker label got expected () =
  Alcotest.(check @@ result checker string )
    label
    got
    expected

let check_result checker label got expected () =
  Alcotest.(check @@ result checker string)
    label
    got
    expected

let tc
    ~label
    ?(speed=`Quick)
    ?(descr="")
    checker
    got
    expected =
  Alcotest.(
    test_case
      label
      speed
      (check_result checker descr got expected)
  )

(*
let succeed =
    ~label
    ~descr
    checker
    decoder
    content
    output =

    tc
      ~label ~descr
      checker
      Decoder.(decode_string decoder content)
      output
*)


let primitives =
  [ tc
      ~label:"int"
      ~descr:"decode a natural int"
      Alcotest.int
      Decoder.(decode_string int "23")
      (Result.Ok 23)
  ; tc
      ~label:"int"
      ~descr:"decode a negative int"
      Alcotest.int
      Decoder.(decode_string int "-46")
      ( Result.Ok ~-46 )
  ; tc
      ~label:"float"
      ~descr:"decode a float with some precision"
      Alcotest.(float 1.)
      Decoder.(decode_string float "2.3")
      ( Result.Ok 2.3 )
  ; tc
      ~label:"string"
      ~descr:"decodes a plain string"
      Alcotest.string
      Decoder.(decode_string string "\"twenty-three\"")
      ( Result.Ok "twenty-three" )
  ; tc
      ~label:"true"
      ~descr:"decodes a true bool"
      Alcotest.bool
      Decoder.(decode_string bool "true")
      ( Result.Ok true )
  ; tc
      ~label:"false"
      ~descr:"decodes a false bool"
      Alcotest.bool
      Decoder.(decode_string bool "false")
      ( Result.Ok false )
  ]

let containers =
  [ tc
      ~label:"list-int-index"
      Alcotest.int
      Decoder.(decode_string (index 1 int) "[1,48,3]")
      ( Result.Ok 48 )
  ; tc
      ~label:"dict-field"
      Alcotest.(float 1.)
      Decoder.(decode_string (field "lat" float) "{\"lat\": 52.3}")
      ( Result.Ok 52.3 )
  ; tc
      ~label:"dict-field"
      Alcotest.(float 1.)
      Decoder.(decode_string (field "lng" float) "{\"lat\": 52.3}")
      ( Result.Error "key lng does not exist in object lat " )
  ; tc
      ~label:"list"
      Alcotest.(list int)
      Decoder.(decode_string (list int) "[1,48,3]")
      ( Result.Ok [1;48;3] )
  ; tc
      ~label:"array"
      Alcotest.(array int)
      Decoder.(decode_string (array int) "[1,48,3]")
      ( Result.Ok [|1;48;3|] )
  ; tc
      ~label:"pairs"
      Alcotest.(list (pair string int))
      Decoder.(decode_string (pairs int) "{\"lat\": 5, \"lng\": 15}")
      ( Result.Ok ["lat",5; "lng",15] )
  ; tc
      ~label:"mapN"
      Alcotest.string
      Decoder.(decode_string (
          mapN (Printf.sprintf "{lat:%d, lng:%d}")
          ||> "lat" @= int
          ||> "lng" @= int
        )
          "{\"lat\": 5, \"lng\": 15}"
        )
      ( Result.Ok "{lat:5, lng:15}" )
  ]



(*
  [ succeed
      ~label:"int"
      ~descr:"decode a plain int"
      Alcotest.int
      Decoder.int "23" 23
  ]
   *)
let () =
    Alcotest.run "json_decoder"
    [ "primitives", primitives
    ; "containers", containers
    ]
