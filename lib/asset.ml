type country = FR | US

type asset_type =
  | Stock

type company = {
  name : string;
  desc : string;
  from : country
}

type exchange = {
  code : string;
  name : string;
  from : country
}

type asset =
  | Stock of {
    code          : string;
    name          : string;
    company_code  : string;
    exchange_code : string
  }

module Country = struct
  let to_json = function
    | FR -> `String "FR"
    | US -> `String "US"

  let from_json = function
    | `String s when s="FR" -> FR
    | `String s when s="US" -> US
    | _ -> failwith "country_of_json: invalid country."
end

module Company = struct
  let to_json = function
    | {name;desc;from} ->
      `Assoc
      [ ("name", `String name)
      ; ("desc", `String desc)
      ; ("from", Country.to_json from)
      ]

  let from_json = function
    | `Assoc
      [ ("name", `String name)
      ; ("desc", `String desc)
      ; ("from", c)
      ] ->
      {name=name; desc=desc; from=Country.from_json c}
    | _ -> failwith "company_of_json: invalid company."
end

module Exchange = struct
  let to_json = function
    | {code;name;from} ->
      `Assoc
      [ ("code", `String code)
      ; ("name", `String name)
      ; ("from", Country.to_json from)
      ]

  let from_json = function
    | `Assoc
      [ ("code", `String code)
      ; ("name", `String name)
      ; ("from", c)
      ] ->
      {code=code; name=name; from=Country.from_json c}
    | _ -> failwith "exchange_of_json: invalid exchange."
end

let to_json = function
  | Stock {code;name;company_code;exchange_code} ->
    `Assoc
    [ ("asset_type", `String "stock")
    ; ("code", `String code)
    ; ("name", `String name)
    ; ("company_code", `String company_code)
    ; ("exchange_code", `String exchange_code)
    ]

let from_json = function
  | `Assoc
    [ ("asset_type", `String s)
    ; ("code", `String code)
    ; ("name", `String name)
    ; ( "company_code", `String c)
    ; ("exchange_code", `String e)
    ]
    when s="stock" ->
    Stock
    { code=code
    ; name=name
    ; company_code=c
    ; exchange_code=e
    }
  | _ -> failwith "asset_of_json: invalid asset."
