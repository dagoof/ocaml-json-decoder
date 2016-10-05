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
    val option : 'a t -> 'a option t
    val list : 'a t -> 'a list t
    val field : string -> 'a t -> 'a t
    val (@=) : string -> 'a t -> 'a t
    val at : string list -> 'a t -> 'a t
    val index : int -> 'a t -> 'a t
    val one_of : 'a t list -> 'a t
    val succeed : 'a -> 'a t
    val fail : string -> 'a t
    val map : ( 'a -> 'b ) -> 'a t -> 'b t
    val and_then : ('a -> 'b t) -> 'a t -> 'b t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    val (<$>) : ('a -> 'b) -> 'a t -> 'b t
    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
    val (|.) : ('a -> 'b) t -> 'a t -> 'b t
    val decode : 'a t -> value -> ( 'a, string ) Result.result
    val value : string -> value
    val value_of_yojson : Yojson.Basic.json -> value
    val decode_string : 'a t -> string -> ( 'a, string ) Result.result

end = struct
    type value = Yojson.Basic.json

    let describe_value = function
        | `String v -> "string"
        | `Float v -> "float"
        | `Int v -> "int"
        | `Bool v -> "bool"
        | `Null -> "null"
        | `Assoc v -> "object"
        | `List v -> "list"

    let value_error s value =
        describe_value value
        |> Printf.sprintf s
        |> ( fun s -> Result.Error s )

    type 'a t =
        | Decoder of ( value -> ('a, string) Result.result )

    let decode ( Decoder f ) value = f value

    let string =
        Decoder begin function
            | `String s -> Result.Ok s
            | otherwise -> value_error "%s is not a string" otherwise
        end

    let float =
        Decoder begin function
            | `Float f -> Result.Ok f
            | otherwise -> value_error "%s is not a float" otherwise
        end

    let int =
        Decoder begin function
            | `Int i -> Result.Ok i
            | otherwise -> value_error "%s is not a int" otherwise
        end

    let bool =
        Decoder begin function
            | `Bool b -> Result.Ok b
            | otherwise -> value_error "%s is not a bool" otherwise
        end

    let null a =
        Decoder begin function
            | `Null -> Result.Ok a
            | otherwise -> value_error "%s is not null" otherwise
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
            | otherwise -> value_error "%s is not a list" otherwise
        end

    let field key decoder =
        Decoder begin function
            | `Assoc a ->
                let value = try
                    decode decoder @@ List.assoc key a
                with 
                    Not_found ->
                        let keys =
                            a
                            |> List.map fst
                            |> String.concat ", "
                        in
                        Result.Error ( Printf.sprintf "key %s does not exist in object %s " key keys)
                in
                value
            | otherwise -> value_error "%s is not an object" otherwise
        end

    let (@=) = field

    let at keys decoder =
        List.fold_right field keys decoder

    let index i decoder =
        Decoder begin function
            | `List values ->
                let value = try
                    decode decoder @@ List.nth values i
                with Failure e ->
                    Result.Error "index out of bounds"
                in
                value
            | otherwise -> value_error "%s is not a list" otherwise
        end


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

    let option decoder =
        Decoder begin fun value ->
            let opt =
                match decode decoder value with
                | Result.Ok c -> Some c
                | Result.Error e -> None
            in
            Result.Ok opt
        end

    let and_then fn decoder =
        Decoder begin fun value ->
            let ( Decoder callback ) = 
                match decode decoder value with
                | Result.Ok c -> fn c
                | Result.Error e -> fail e
            in
            callback value
        end

    let map f decoder =
        let open ResultC in
        Decoder begin fun value ->
            decode decoder value >>| f
        end

    let (>>=) decoder fn = and_then fn decoder

    let (>>|) t f = map f t

    let (<$>) = map

    let (<*>) f t = f >>= fun f -> t >>| f

    let (|.) = (<*>)

    let apply f t = f <*> t

    let value s = Yojson.Basic.from_string s

    let value_of_yojson v = v
    
    let decode_string t s =
        decode t @@ value s 
end

module Obj = struct
    open Decoder

    let object1 fn a =
        fn <$> a

    let object2 fn a b =
        fn <$> a <*> b

    let object3 fn a b c =
        fn <$> a <*> b <*> c

    let object4 fn a b c d =
        fn <$> a <*> b <*> c <*> d

    let object5 fn a b c d e =
        fn <$> a <*> b <*> c <*> d <*> e

    let object6 fn a b c d e f =
        fn <$> a <*> b <*> c <*> d <*> e <*> f

    let object7 fn a b c d e f g =
        fn <$> a <*> b <*> c <*> d <*> e <*> f <*> g

    let object8 fn a b c d e f g h =
        fn <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h

    let object9 fn a b c d e f g h i =
        fn <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i

end

include Obj
