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

let json_of_country = function
  | FR -> `String "FR"
  | US -> `String "US"

let json_of_assetinfo = function
  | ({code;name} : asset_info) ->
    `Assoc [
      ("code", `String code);
      ("name", `String name)
    ]

let json_of_company = function
  | {name;desc;from} ->
    `Assoc [
    ("name", `String name);
    ("desc", `String desc);
    ("from", json_of_country from)
  ]

let json_of_exchange = function
  | {code;name;from} ->
    `Assoc [
    ("code", `String code);
    ("name", `String name);
    ("from", json_of_country from)
  ]

let json_of_asset = function
  | Stock (info, c, e) ->
    `Assoc [
    ("asset_info", json_of_assetinfo info);
    ("company", json_of_company c);
    ("exchange", json_of_exchange e)
  ]
