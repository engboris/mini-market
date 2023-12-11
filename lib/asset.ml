(* ============================
   Asset
   ============================
 * \jsonof
 * \ofjson
 *)

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

(* ----------------------------
   Conversion to JSON
   ----------------------------
 \jsonof *)

let json_of_country = function
  | FR -> `String "FR"
  | US -> `String "US"

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
  | Stock {code;name;company_code;exchange_code} ->
    `Assoc [
    ("asset_type", `String "stock");
    ("code", `String code);
    ("name", `String name);
    ("company_code", `String company_code);
    ("exchange_code", `String exchange_code)
  ]

(* ----------------------------
   Conversion from JSON
   ----------------------------
 \jsonof *)

let country_of_json = function
  | `String s when s="FR" -> FR
  | `String s when s="US" -> US
  | _ -> failwith "country_of_json: invalid country."

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
    ("code", `String code);
    ("name", `String name);
    ("company_code", `String c);
    ("exchange_code", `String e)
    ]
    when s="stock" ->
    Stock {
      code=code;
      name=name;
      company_code=c;
      exchange_code=e
    }
  | _ -> failwith "asset_of_json: invalid asset."
