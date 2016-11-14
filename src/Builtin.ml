
module Value =
  struct
    type t = Int of int | String of bytes

    let of_int x = Int x
    let of_string b = String b
    let to_int (Int x) = x
    let to_string (String b) = b

    let str x = 
        match x with
        | Int x -> Printf.sprintf "%d" x
        | String s -> Printf.sprintf "%s" s
  end

let read (x::input') output = (x, input', output)

let write x input output = (Value.Int 0, input, output@[x])

let strmake (Value.String c) (Value.Int n) = Value.String (Bytes.make n (Bytes.get c 0))

let strset (Value.String b) (Value.Int i) (Value.String c) = 
    Bytes.set b i (Bytes.get c 0);
    Value.String b

let strdup (Value.String b) = Value.String (Bytes.copy b)

let strcat (Value.String b1) (Value.String b2) = Value.String (Bytes.cat b1 b2) 

let strcmp (Value.String b1) (Value.String b2) = Value.Int (Bytes.compare b1 b2) 

let strlen (Value.String b) = Value.Int (Bytes.length b)

let strsub (Value.String b) (Value.Int i) (Value.Int l) = Value.String (Bytes.sub b i l)

let strget s i = strsub s i (Value.Int 1)

