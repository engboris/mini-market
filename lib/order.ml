open Base

type side_type = Buy | Sell

type order_type = Market | Limit of int

type t = {
  asset_code : string;
  ordtype    : order_type;
  side       : side_type;
  size       : int;
  t          : Datetime.datetime;
  user_id    : int
}

module Side = struct
  let to_string = function
    | Buy -> "buy"
    | Sell -> "sell"

  let from_string s =
    if equal_string s "buy" then Buy
    else if equal_string s "sell" then Sell
    else failwith "Side.from_string: invalid side."

  let to_json st : Yojson.Basic.t =
    `String (to_string st)

  let from_json = function
    | `String "buy" -> Buy
    | `String "sell" -> Sell
    | _ -> failwith "Side.from_json: not a valid order side."
  
  end

module Type = struct
  let to_json : order_type -> Yojson.Basic.t = function
    | Market -> `Assoc [("type", `String "market")]
    | Limit n -> `Assoc [("type", `String "limit"); ("limit", `Int n)]
    
  let from_json = function
    | `Assoc [("type", `String "market")] -> Market
    | `Assoc [("type", `String "limit"); ("limit", `Int n)] ->
      Limit n
    | _ -> failwith "Type.from_json: not a valid order type."
end

let to_json : t -> Yojson.Basic.t = function
  | {asset_code;ordtype;side;size;t;user_id} ->
    `Assoc
    [ ("asset_code", `String asset_code)
    ; ("ordtype", Type.to_json ordtype)
    ; ("side", Side.to_json side)
    ; ("size", `Int size)
    ; ("t", `String (Datetime.to_string t))
    ; ("user_id", `Int user_id)
    ]

let from_json : Yojson.Basic.t -> t = function
  | `Assoc
    [ ("asset_code", `String asset_code)
    ; ("ordtype", ordtype)
    ; ("side", side)
    ; ("size", `Int size)
    ; ("t", `String dt)
    ; ("user_id", `Int user_id)
    ] ->
    { asset_code = asset_code
    ; ordtype = Type.from_json ordtype
    ; side = Side.from_json side
    ; size = size
    ; t = Datetime.of_string dt
    ; user_id = user_id
    }
  | _ -> failwith "order_of_json: not a valid order."
