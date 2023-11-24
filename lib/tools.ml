let pad (max : int) (c : char) (s : string) =
  let size = String.length s in
  let sc   = String.make 1 c in
  let rec aux k s =
    if k=0 || size>=max then s 
    else aux (k-1) (sc^s) 
  in aux (max-size) s

let insert_string s (l : string list) =
  List.fold_right (fun x bs ->
    if bs="" then x else x^s^bs
  ) l ""

