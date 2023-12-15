open Base

let users_path = "data/users.json"
let orderbooks_path = "data/orderbooks/"

let new_order side ordtype size asset id : Order.t =
  let dt = Datetime.current () in
  { asset_code = asset
  ; ordtype = ordtype
  ; side = side
  ; size = size
  ; t = Datetime.of_string dt
  ; user_id = id
  }

let initialise_json path =
  let oc =
    Out_channel.open_gen [Open_wronly; Open_creat] 0o777 path in
  Out_channel.output_string oc "[]";
  Out_channel.close oc

let load_users () =
  Tools.get_file_content users_path
  |> Yojson.Basic.from_string 
  |> function
    | `List l -> List.map ~f:User.from_json l
    | _ -> failwith "load_users: invalid list of users."

let load_orderbook () =
  let d = Datetime.Date.current () in
  let path = orderbooks_path^d^".json" in 
  (if not (Stdlib.Sys.file_exists path) then
    initialise_json path);
  Tools.get_file_content path
  |> Yojson.Basic.from_string
  |> function
    | `List l -> List.map ~f:Order.from_json l
    | _ -> failwith "load_orderbook: invalid orderbook."

let save_orderbook orderbook =
  let j = `List (List.map ~f:Order.to_json orderbook) in
  let d = Datetime.Date.current () in
  let path = orderbooks_path^d^".json" in
  let oc =
    Out_channel.open_gen [Open_wronly; Open_trunc] 777 path in
  Out_channel.output_string oc (Yojson.Basic.to_string j);
  Out_channel.close oc

