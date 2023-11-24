(* ============================
   Datetime
   ============================
 * \date
 * \time
 *)

type date = {
  dd   : int;
  mm   : int;
  yyyy : int;
}

type time = {
  h : int;
  m : int;
  s : int
}

type datetime = date * time

(* ----------------------------
   Date
   ----------------------------
 \date *)

let rec update_date : date -> date = function
  | {dd;mm;yyyy} as d ->
      if (dd>31 && mm>=1 && mm<=7 && mm mod 2 = 1) ||
         (dd>31 && (mm=8||mm=10||mm=12)) ||
         (dd>30 && (mm=4||mm=6||mm=9||mm=11)) ||
         (dd>28 && mm=2 && yyyy mod 4 != 0) ||
         (dd>29 && mm=2 && yyyy mod 4 = 0) then
        update_date {dd=1;mm=mm+1;yyyy}
      else if mm>12 then
        update_date {dd=1;mm=1;yyyy=yyyy+1}
      else
        d

let next_day : date -> date = function
  | {dd;mm;yyyy} ->
    update_date {dd=dd+1;mm=mm;yyyy=yyyy}

let string_of_date : date -> string = function
  | {dd;mm;yyyy} ->
    [dd;mm;yyyy]
    |> List.map string_of_int
    |> List.map (Tools.pad 2 '0')
    |> Tools.insert_string "/"

let date_of_string (s : string) : date =
  match String.split_on_char '/' s |> List.map int_of_string with
  | [dd; mm; yyyy] -> {dd=dd; mm=mm; yyyy=yyyy}
  | _         -> failwith "date_of_string: not a well-formed date."

(* ----------------------------
   Time
   ----------------------------
 \time *)

let rec update_time : time -> time = function
  | {h;m;s} as t ->
    if s>59 then update_time {h=h;m=m+1;s=0}
    else if m>59 then update_time {h=h+1;m=0;s=0}
    else if h>23 then {h=0;m=0;s=0}
      else t

let tick : time -> time = function
  | {h;m;s} -> update_time {h=h;m=m;s=s+1}

let string_of_time : time -> string = function
  | {h;m;s} ->
    [h;m;s]
    |> List.map string_of_int
    |> List.map (Tools.pad 2 '0')
    |> Tools.insert_string ":" 

let time_of_string (s : string) : time =
  match String.split_on_char ':' s |> List.map int_of_string with
  | [h; m; s] -> {h=h; m=m; s=s}
  | _         -> failwith "time_of_string: not a well-formed time."
