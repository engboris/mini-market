open Base

type order_type =
  | Market
  | Limit of int

type order = {
  asset_code : string;
  ordtype    : order_type;
  size       : int;
  t          : Datetime.datetime;
  user_id    : int 
}

module Type = struct
  let to_json : order_type -> Yojson.Basic.t = function
    | Market -> `Assoc [("type", `String "market")]
    | Limit n -> `Assoc [("type", `String "limit"); ("limit", `Int n)]
    
  let from_json = function
    | `Assoc [("type", `String "market")] -> Market
    | `Assoc [("type", `String "limit"); ("limit", `Int n)] ->
      Limit n
    | _ -> failwith "ordtype_of_json: not a valid order type."
end

let to_json : order -> Yojson.Basic.t = function
  | {asset_code;ordtype;size;t;user_id} ->
    `Assoc
    [ ("asset_code", `String asset_code)
    ; ("ordtype", Type.to_json ordtype)
    ; ("size", `Int size)
    ; ("t", `String (Datetime.to_string t))
    ; ("user_id", `Int user_id)
    ]

let from_json : Yojson.Basic.t -> order = function
  | `Assoc
    [ ("asset_code", `String asset_code)
    ; ("ordtype", ordtype)
    ; ("size", `Int size)
    ; ("t", `String dt)
    ; ("user_id", `Int user_id)
    ] ->
    { asset_code = asset_code
    ; ordtype = Type.from_json ordtype
    ; size = size
    ; t = Datetime.of_string dt
    ; user_id = user_id
    }
  | _ -> failwith "order_of_json: not a valid order."
