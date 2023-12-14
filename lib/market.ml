open Base

let users_path = "data/users.json"
let orderbooks_path = "data/orderbooks/"

type user = {
  id           : int;
  username     : string;
  email        : string;
  password     : string;
  balance      : int;
  assets_value : int;
  assets       : Asset.asset list
}

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

(* ----------------------------
   Conversion from JSON
   ----------------------------
 \jsonof *)

let json_of_user : user -> Yojson.Basic.t = function
  | {id;username;email;password;balance;assets_value;assets} ->
    `Assoc
    [ ("id", `Int id)
    ; ("username", `String username)
    ; ("email", `String email)
    ; ("password", `String password)
    ; ("balance", `Int balance)
    ; ("assets_value", `Int assets_value)
    ; ("assets", `List (List.map ~f:Asset.json_of_asset assets))
    ]

let json_of_ordertype : order_type -> Yojson.Basic.t = function
  | Market -> `Assoc [("type", `String "market")]
  | Limit n -> `Assoc [("type", `String "limit"); ("limit", `Int n)]

let json_of_order : order -> Yojson.Basic.t = function
  | {asset_code;ordtype;size;t;user_id} ->
    `Assoc
    [ ("asset_code", `String asset_code)
    ; ("ordtype", json_of_ordertype ordtype)
    ; ("size", `Int size)
    ; ("t", `String (Datetime.to_string t))
    ; ("user_id", `Int user_id)
    ]

(* ----------------------------
   Conversion to JSON
   ----------------------------
 \ofjson *)

let ordtype_of_json = function
  | `Assoc [("type", `String "market")] -> Market
  | `Assoc [("type", `String "limit"); ("limit", `Int n)] ->
    Limit n
  | _ -> failwith "ordtype_of_json: not a valid order type."

let user_of_json : Yojson.Basic.t -> user = function
  | `Assoc
    [ ("id", `Int id)
    ; ("username", `String username)
    ; ("email", `String email)
    ; ("password", `String password)
    ; ("balance", `Int balance)
    ; ("assets_value", `Int assets_value)
    ; ("assets", `List assets)
    ] ->
    { id=id
    ; username=username
    ; email=email
    ; password=password
    ; balance=balance
    ; assets_value=assets_value
    ; assets=(List.map ~f:Asset.asset_of_json assets)
    }
  | _ -> failwith "user_of_json: not a valid user."

let order_of_json : Yojson.Basic.t -> order = function
  | `Assoc
    [ ("asset_code", `String asset_code)
    ; ("ordtype", ordtype)
    ; ("size", `Int size)
    ; ("t", `String dt)
    ; ("user_id", `Int user_id)
    ] ->
    { asset_code = asset_code
    ; ordtype = ordtype_of_json ordtype
    ; size = size
    ; t = Datetime.of_string dt
    ; user_id = user_id
    }
  | _ -> failwith "order_of_json: not a valid order."

(* ----------------------------
   Data management
   ----------------------------
 \dataload *)

let initialise_json path =
  let oc =
    Out_channel.open_gen [Open_wronly; Open_creat] 0o777 path in
  Out_channel.output_string oc "[]";
  Out_channel.close oc

let load_users () =
  Tools.get_file_content users_path
  |> Yojson.Basic.from_string 
  |> function
    | `List l -> List.map ~f:user_of_json l
    | _ -> failwith "load_users: invalid list of users."

let load_orderbook () =
  let d = Datetime.Date.current () in
  let path = orderbooks_path^d^".json" in 
  (if not (Stdlib.Sys.file_exists path) then
    initialise_json path);
  Tools.get_file_content path
  |> Yojson.Basic.from_string
  |> function
    | `List l -> List.map ~f:order_of_json l
    | _ -> failwith "load_orderbook: invalid orderbook."

let save_orderbook orderbook =
  let j = `List (List.map ~f:json_of_order orderbook) in
  let d = Datetime.Date.current () in
  let path = orderbooks_path^d^".json" in
  let oc =
    Out_channel.open_gen [Open_wronly; Open_trunc] 777 path in
  Out_channel.output_string oc (Yojson.Basic.to_string j);
  Out_channel.close oc

