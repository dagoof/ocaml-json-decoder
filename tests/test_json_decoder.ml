open Json_decoder

let person () =
    let open Decoder in
    let decoder = "age" @= int in
    let decoded = decode_string decoder "{\"age\": 28}" in
    Alcotest.(check @@ result int string)
        "person age"
        ( Result.Ok 28 )
        decoded

let simple_decoders =
    [ "Person", `Quick, person
    ; "Person1", `Slow, person
    ; "Person2", `Slow, person
    ; "Person3", `Quick, person
    ]


let () =
    Alcotest.run "test suite"
    [ "simple_decoders", simple_decoders
    ]
