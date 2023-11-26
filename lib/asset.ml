(* ============================
   Asset
   ============================
 * \json_of
 * \of_json
 *)

type country = FR | US

type asset_type =
  | Stock

type asset_info = {
  code  : string;
  name  : string;
}

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
  | Stock of (asset_info * company * exchange) 

(* ----------------------------
   Conversion to JSON
   ----------------------------
 \json *)

let json_of_country = function
  | FR -> `String "FR"
  | US -> `String "US"

let json_of_assetinfo = function
  | ({code=code;name=name} : asset_info) ->
    `Assoc [
      ("code", `String code);
      ("name", `String name)
    ]

let json_of_company = function
  | {name=name;desc=desc;from=from} ->
    `Assoc [
    ("name", `String name);
    ("desc", `String desc);
    ("from", json_of_country from)
  ]

let json_of_exchange = function
  | {code=code;name=name;from=from} ->
    `Assoc [
    ("code", `String code);
    ("name", `String name);
    ("from", json_of_country from)
  ]

let json_of_asset = function
  | Stock (info, c, e) ->
    `Assoc [
    ("asset_type", `String "stock");
    ("asset_info", json_of_assetinfo info);
    ("company", json_of_company c);
    ("exchange", json_of_exchange e)
  ]

(* ----------------------------
   Conversion from JSON
   ----------------------------
 \fromjson *)

let country_of_json = function
  | `String s when s="FR" -> FR
  | `String s when s="US" -> US
  | _ -> failwith "country_of_json: invalid country."

let assetinfo_of_json = function
  | `Assoc [
    ("code", `String code);
    ("name", `String name)
    ] ->
    {code=code; name=name}
  | _ -> failwith "assetinfo_of_json: invalid asset_info."


let company_of_json = function
  | `Assoc [
    ("name", `String name);
    ("desc", `String desc);
    ("from", c)
    ] ->
    {name=name; desc=desc; from=country_of_json c}
  | _ -> failwith "company_of_json: invalid company."

let exchange_of_json = function
  | `Assoc [
    ("code", `String code);
    ("name", `String name);
    ("from", c)
    ] ->
    {code=code; name=name; from=country_of_json c}
  | _ -> failwith "exchange_of_json: invalid exchange."

let asset_of_json = function
  | `Assoc [
    ("asset_type", `String s);
    ("asset_info", i);
    ("company", c);
    ("exchange", e)
    ]
    when s="stock" ->
    Stock (
      assetinfo_of_json i,
      company_of_json c,
      exchange_of_json e
    )
  | _ -> failwith "asset_of_json: invalid asset."
