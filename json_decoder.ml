open Yojson

let always x y = x

module ResultC = struct
    let (>>=) = Rresult.R.bind

    let (>>|) t f = Rresult.R.map f t

    let (>>!) t f = Rresult.R.reword_error f t

    let (<*>) f t = f >>= fun f -> t >>| f

    let (<$>) f t = t >>| f
end

module Decoder : sig 
    type 'a t
    type value

    val string : string t
    val float : float t
    val int : int t
    val bool : bool t
    val null : 'a -> 'a t
    val list : 'a t -> 'a list t
    val key : string -> 'a t -> 'a t
    val (|=) : string -> 'a t -> 'a t
    val one_of : 'a t list -> 'a t
    val succeed : 'a -> 'a t
    val fail : string -> 'a t
    val map : ( 'a -> 'b ) -> 'a t -> 'b t
    val and_then : ('a -> 'b t) -> 'a t -> 'b t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

    (*
    val array : 'a -> 'a array t
    val at : list string -> 'a t -> 'a t
    val (<$>) : ('a -> 'b) -> 'a t -> 'b t
    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
    *)

    val decode : 'a t -> value -> ( 'a, string ) Result.result
    val value : string -> value
    val value_of_yojson : Yojson.Safe.json -> value
    val decode_string : 'a t -> string -> ( 'a, string ) Result.result
end = struct
    type value = Yojson.Safe.json

    type 'a t =
        | Decoder of ( value -> ('a, string) Result.result )

    let decode ( Decoder f ) value = f value

    let string =
        Decoder begin function
            | `String s -> Result.Ok s
            | otherwise -> Result.Error "thing is not a string"
        end

    let float =
        Decoder begin function
            | `Float f -> Result.Ok f
            | otherwise -> Result.Error "thing is not a float"
        end

    let int =
        Decoder begin function
            | `Int i -> Result.Ok i
            | otherwise -> Result.Error "thing is not a int"
        end

    let bool =
        Decoder begin function
            | `Bool b -> Result.Ok b
            | otherwise -> Result.Error "thing is not a bool"
        end

    let null a =
        Decoder begin function
            | `Null -> Result.Ok a
            | otherwise -> Result.Error "thing is not null"
        end

    let list decoder =
        Decoder begin function
            | `List values ->
                let open ResultC in
                let push x xs = x :: xs in

                List.fold_left begin fun acc value ->
                    push <$> decode decoder value <*> acc
                end
                ( Result.Ok [] )
                values
                >>| List.rev
            | otherwise -> Result.Error "thing is not a list"
        end

    let key label decoder =
        Decoder begin function
            | `Assoc a ->
                let value = try
                    decode decoder @@ List.assoc label a
                with 
                    Not_found ->
                        Result.Error ( Printf.sprintf "key %s does not exist in object" label )
                in
                value
            | otherwise -> Result.Error "thing is not an object"
        end

    let (|=) = key

    let one_of decoders =
        Decoder begin fun value ->
            let values =
                List.map ( fun decoder -> decode decoder value ) decoders in
            try
                List.find Rresult.R.is_ok values
            with
                Not_found -> Result.Error "no suitable decoder chosen"
        end

    let succeed a = Decoder ( always @@ Result.Ok a )

    let fail s = Decoder ( always @@ Result.Error s )

    let and_then fn decoder =
        let open ResultC in
        Decoder begin fun value ->
            let ( Decoder callback ) = 
                match decode decoder value with
                | Result.Ok c -> fn c
                | Result.Error e -> fail e
            in
            callback value
        end

    let (>>=) decoder fn = and_then fn decoder

        (*
    let and_thenx decoder fn =
        let open ResultC in
        Decoder begin fun value ->
            match decode decoder value with
            | Result.Ok a -> Decoder begin fun v2 ->
                ( fn a ) v2
            end
            (* decode decoder value >>= fun a ->  *)
        end
*)

        (*
        let open ResultC in
        Decoder begin fun value ->
            'a t -> ('a -> 'b t) -> 'b t

            decoder string
            string -> decoder int
            -> decoder int

            value >>= 

            decode decoder value -- result a>>= ( decode ( Decoder ( fun a -> fn a )))
        *)

    let map f decoder =
        let open ResultC in
        Decoder begin fun value ->
            decode decoder value >>| f
        end

    let value s = Yojson.Safe.from_string s

    let value_of_yojson v = v
    
    let decode_string t s =
        decode t @@ value s 
end



let () =
    let open Decoder in

    let decoder = one_of
        [ "frogx" |= Decoder.int 
        ; "frog" |= Decoder.int
        ; "frogxd" |= Decoder.string >>= ( fun x -> "freog" |= Decoder.int )
        ; Decoder.map int_of_string ( "frogxd" |= Decoder.string ) 
        ; Decoder.succeed 500
        ]
    in
    Printf.printf "SUCCESS: %d\n" @@
    Rresult.R.get_ok @@
    Decoder.decode decoder @@
    Decoder.value @@
    "{\"frogxd\": \"15\", \"freog\": 233}"

    (* "[\"fine dude\", \"you know it\"]" *)
