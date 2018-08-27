open Json_decoder

let check_result checkable label got expected () =
  Alcotest.(check @@ result checkable string)
    label
    got
    expected

let test_case
    ~label
    ?(speed=`Quick)
    ?(descr="")
    checkable
    got
    expected =
  Alcotest.(
    test_case
      label
      speed
      (check_result checkable descr got expected)
  )

let succeed
    ?(speed=`Quick)
    ?(descr="")
    checkable
    decoder
    source
    output =
  test_case
    ~speed
    ~descr
    checkable
    Decoder.(decode_string decoder source)
    (Result.Ok output)

let primitives =
  [ test_case
      ~label:"int"
      ~descr:"decode a natural int"
      Alcotest.int
      Decoder.(decode_string int "23")
      (Result.Ok 23)
  ; succeed
      ~label:"int wow"
      Alcotest.int
      Decoder.int "23" 23
  ; test_case
      ~label:"int"
      ~descr:"decode a negative int"
      Alcotest.int
      Decoder.(decode_string int "-46")
      ( Result.Ok ~-46 )
  ; test_case
      ~label:"float"
      ~descr:"decode a float with some precision"
      Alcotest.(float 1.)
      Decoder.(decode_string float "2.3")
      ( Result.Ok 2.3 )
  ; test_case
      ~label:"string"
      ~descr:"decodes a plain string"
      Alcotest.string
      Decoder.(decode_string string "\"twenty-three\"")
      ( Result.Ok "twenty-three" )
  ; test_case
      ~label:"true"
      ~descr:"decodes a true bool"
      Alcotest.bool
      Decoder.(decode_string bool "true")
      ( Result.Ok true )
  ; test_case
      ~label:"false"
      ~descr:"decodes a false bool"
      Alcotest.bool
      Decoder.(decode_string bool "false")
      ( Result.Ok false )
  ]

let containers =
  [ test_case
      ~label:"list-int-index"
      Alcotest.int
      Decoder.(decode_string (index 1 int) "[1,48,3]")
      ( Result.Ok 48 )
  ; test_case
      ~label:"dict-field"
      Alcotest.(float 1.)
      Decoder.(decode_string (field "lat" float) "{\"lat\": 52.3}")
      ( Result.Ok 52.3 )
  ; test_case
      ~label:"dict-field"
      Alcotest.(float 1.)
      Decoder.(decode_string (field "lng" float) "{\"lat\": 52.3}")
      ( Result.Error "key lng does not exist in object lat " )
  ; test_case
      ~label:"list"
      Alcotest.(list int)
      Decoder.(decode_string (list int) "[1,48,3]")
      ( Result.Ok [1;48;3] )
  ; test_case
      ~label:"array"
      Alcotest.(array int)
      Decoder.(decode_string (array int) "[1,48,3]")
      ( Result.Ok [|1;48;3|] )
  ; test_case
      ~label:"pairs"
      Alcotest.(list (pair string int))
      Decoder.(decode_string (pairs int) "{\"lat\": 5, \"lng\": 15}")
      ( Result.Ok ["lat",5; "lng",15] )
  ; test_case
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



let () =
    Alcotest.run "json_decoder"
    [ "primitives", primitives
    ; "containers", containers
    ]
