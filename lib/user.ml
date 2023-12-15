open Base

type t = {
  id           : int;
  username     : string;
  email        : string;
  password     : string;
  balance      : int;
  assets_value : int;
  assets       : Asset.asset list
}

let to_json : t -> Yojson.Basic.t = function
  | {id;username;email;password;balance;assets_value;assets} ->
    `Assoc
    [ ("id", `Int id)
    ; ("username", `String username)
    ; ("email", `String email)
    ; ("password", `String password)
    ; ("balance", `Int balance)
    ; ("assets_value", `Int assets_value)
    ; ("assets", `List (List.map ~f:Asset.to_json assets))
    ]

let from_json : Yojson.Basic.t -> t = function
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
    ; assets=(List.map ~f:Asset.from_json assets)
    }
  | _ -> failwith "user_of_json: not a valid user."
