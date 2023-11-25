
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
  balance      : int;
  assets_value : int;
  assets       : Asset.asset list
}

let json_of_user = function
  | {id;username;email;balance;assets_value;assets} ->
    `Assoc [
      ("id", `Int id);
      ("username", `String username);
      ("email", `String email);
      ("balance", `Int balance);
      ("assets_value", `Int assets_value);
      ("assets", `List (List.map Asset.json_of_asset assets))
    ]

let user_of_json = function
  | `Assoc [
      ("id", `Int id);
      ("username", `String username);
      ("email", `String email);
      ("balance", `Int balance);
      ("assets_value", `Int assets_value);
      ("assets", `List assets)
    ] ->
    {id;username;email;balance;assets_value;assets}
  | _ -> failwith "user_of_json: not a valid user."

let initialise_json path =
  let oc = open_out_gen [Open_wronly; Open_creat] 777 path in
  Printf.fprintf oc "[]";
  close_out oc

let start_session () =
  let d = Datetime.current_date () in
  let path = "data/orderbooks/"^d^".json" in 
  (if not (Sys.file_exists path) then
    initialise_json path);
  let ic = open_in path in
  let s = In_channel.input_all ic in
  close_in ic;
  let json = Yojson.Basic.from_string s in
  ()
