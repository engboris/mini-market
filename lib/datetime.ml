open Base

(* ============================
   Datetime
   ============================
 * \date
 * \time
 * \datetime
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

let current_date () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d-%02d-%d"
  tm.tm_mday tm.tm_mon (1900+tm.tm_year)

let current_time () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d:%02d:%02d"
  tm.tm_hour tm.tm_min tm.tm_sec

let current_datetime () =
  current_date () ^ " " ^
  current_time ()

(* ----------------------------
   Date
   ----------------------------
 \date *)

let rec update_date : date -> date = function
  | {dd;mm;yyyy} as d ->
      if (dd>31 && mm>=1 && mm<=7 && mm % 2 = 1) ||
         (dd>31 && (mm=8||mm=10||mm=12)) ||
         (dd>30 && (mm=4||mm=6||mm=9||mm=11)) ||
         (dd>28 && mm=2 && not (yyyy % 4 = 0)) ||
         (dd>29 && mm=2 && yyyy % 4 = 0) then
        update_date {dd=1;mm=mm+1;yyyy=yyyy}
      else if mm>12 then
        update_date {dd=1;mm=1;yyyy=yyyy+1}
      else
        d

let next_day (d:date) : date =
  update_date {d with dd=d.dd+1}

let string_of_date : date -> string = function
  | {dd;mm;yyyy} ->
    [dd;mm;yyyy]
    |> List.map ~f:Int.to_string
    |> List.map ~f:(String.pad_left ~char:'0' ~len:2)
    |> String.concat ~sep:"-"

let date_of_string (s : string) : date =
  match String.split_on_chars ~on:['-'] s
      |>List.map ~f:Int.of_string with
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

let tick (t:time) : time =
  update_time {t with s=t.s+1}

let string_of_time : time -> string = function
  | {h;m;s} ->
    [h;m;s]
    |> List.map ~f:Int.to_string
    |> List.map ~f:(String.pad_left ~char:'0' ~len:2)
    |> String.concat ~sep:":" 

let time_of_string (s : string) : time =
  match String.split_on_chars ~on:[':'] s
      |>List.map ~f:Int.of_string with
  | [h; m; s] -> {h=h; m=m; s=s}
  | _         -> failwith "time_of_string: not a well-formed time."

(* ----------------------------
   Datetime 
   ----------------------------
 \datetime *)

let json_of_datetime = function (d, t) ->
  `Assoc
  [ ("dd", d.dd)
  ; ("mm", d.mm)
  ; ("yyyy", d.yyyy)
  ; ("h", t.h)
  ; ("m", t.m)
  ; ("s", t.s)
  ]

let string_of_datetime = function (d, t) ->
  (string_of_date d) ^ " " ^ (string_of_time t)

let datetime_of_string (s : string) =
  match String.split_on_chars ~on:[' '] s with
  | [sd; st] -> (date_of_string sd, time_of_string st)
  | _ -> failwith "datetime_of_string: invalid datetime."
