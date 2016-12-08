
module Value =
  struct
    type t = Int of int | String of bytes | Array of t array

    let of_int x = Int x
    let to_int (Int x) = x

    let of_string b = String b
    let to_string (String b) = b

    let to_array a = Array a
    let of_array (Array a) = a

    let str x = 
        match x with
        | Int x -> Printf.sprintf "%d" x
        | String s -> Printf.sprintf "%s" s
  end

let read (x::input') output = (x, input', output)

let write x input output = (Value.Int 0, input, output@[x])

let strmake (Value.Int n) (Value.Int c) = Value.String (Bytes.make n (Char.chr c))

let strset (Value.String b) (Value.Int i) (Value.Int c) = 
    Bytes.set b i (Char.chr c);
    Value.String b

let strdup (Value.String b) = Value.String (Bytes.copy b)

let strcat (Value.String b1) (Value.String b2) = Value.String (Bytes.cat b1 b2) 

let strcmp (Value.String b1) (Value.String b2) = Value.Int (Bytes.compare b1 b2) 

let strlen (Value.String b) = Value.Int (Bytes.length b)

let strsub (Value.String b) (Value.Int i) (Value.Int l) = Value.String (Bytes.sub b i l)

let strget (Value.String b) (Value.Int i) = Value.Int (Char.code (Bytes.get b i))

