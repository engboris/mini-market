let users_path = "data/users.json"
let orderbooks_path = "data/orderbooks/"

type order_type =
  | Market
  | Limit of int

type order = {
  content : Asset.asset;
  ordtype : order_type;
  size    : int;
  t       : Datetime.datetime 
}

type user = {
  id           : int;
  username     : string;
  email        : string;
  password     : string;
  balance      : int;
  assets_value : int;
  assets       : Asset.asset list
}

let json_of_user : user -> Yojson.Basic.t = function
  | {id=id;
     username=username;
     email=email;
     password=password;
     balance=balance;
     assets_value=assets_value;
     assets=assets} ->
    `Assoc [
      ("id", `Int id);
      ("username", `String username);
      ("email", `String email);
      ("password", `String password);
      ("balance", `Int balance);
      ("assets_value", `Int assets_value);
      ("assets", `List (List.map Asset.json_of_asset assets))
    ]

let user_of_json : Yojson.Basic.t -> user = function
  | `Assoc [
      ("id", `Int id);
      ("username", `String username);
      ("email", `String email);
      ("password", `String password);
      ("balance", `Int balance);
      ("assets_value", `Int assets_value);
      ("assets", `List assets)
    ] ->
      {
        id=id;
        username=username;
        email=email;
        password=password;
        balance=balance;
        assets_value=assets_value;
        assets=(List.map Asset.asset_of_json assets)
      }
  | _ -> failwith "user_of_json: not a valid user."

let initialise_json path =
  let oc = open_out_gen [Open_wronly; Open_creat] 777 path in
  Printf.fprintf oc "[]";
  close_out oc

let load_users () =
  Tools.get_file_content users_path
  |> Yojson.Basic.from_string 
  |> function
    | `List l -> List.map user_of_json l
    | _ -> failwith "load_users: invalid list of users."

let load_orderbook () =
  let d = Datetime.current_date () in
  let path = orderbooks_path^d^".json" in 
  (if not (Sys.file_exists path) then
    initialise_json path);
  Tools.get_file_content path
  |> Yojson.Basic.from_string
  |> function
    | `List l -> l
    | _ -> failwith "load_orderbook: invalid orderbook."
