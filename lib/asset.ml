type country = FR | US

type asset_type =
  | Stock

type exchange = {
  code : string;
  name : string;
  from : country
}

type company = {
  name : string;
  desc : string;
  from : country
}

type asset_info = {
  code  : string;
  name  : string;
}

type asset =
  | Stock of (asset_info * company * exchange) 
