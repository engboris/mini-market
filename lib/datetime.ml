open Base

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

module Date = struct
  let current () =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "%02d-%02d-%d"
    tm.tm_mday tm.tm_mon (1900+tm.tm_year)

  let rec update : date -> date = function
    | {dd;mm;yyyy} as d ->
      if (dd>31 && mm>=1 && mm<=7 && mm % 2 = 1) ||
         (dd>31 && (mm=8||mm=10||mm=12)) ||
         (dd>30 && (mm=4||mm=6||mm=9||mm=11)) ||
         (dd>28 && mm=2 && not (yyyy % 4 = 0)) ||
         (dd>29 && mm=2 && yyyy % 4 = 0) then
        update {dd=1;mm=mm+1;yyyy=yyyy}
      else if mm>12 then
        update {dd=1;mm=1;yyyy=yyyy+1}
      else
        d

  let next (d:date) : date =
    update {d with dd=d.dd+1}

  let to_string : date -> string = function
    | {dd;mm;yyyy} ->
      [dd;mm;yyyy]
      |> List.map ~f:Int.to_string
      |> List.map ~f:(String.pad_left ~char:'0' ~len:2)
      |> String.concat ~sep:"-"

  let of_string s : date =
    match String.split_on_chars ~on:['-'] s
      |>List.map ~f:Int.of_string with
    | [dd; mm; yyyy] -> {dd=dd; mm=mm; yyyy=yyyy}
    | _         -> failwith "date_of_string: not a well-formed date."
end

module Time = struct
  let current () =
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "%02d:%02d:%02d"
    tm.tm_hour tm.tm_min tm.tm_sec

  let rec update : time -> time = function
    | {h;m;s} as t ->
      if s>59 then update {h=h;m=m+1;s=0}
      else if m>59 then update {h=h+1;m=0;s=0}
      else if h>23 then {h=0;m=0;s=0}
      else t

  let next (t:time) : time =
    update {t with s=t.s+1}

  let to_string : time -> string = function
    | {h;m;s} ->
      [h;m;s]
      |> List.map ~f:Int.to_string
      |> List.map ~f:(String.pad_left ~char:'0' ~len:2)
      |> String.concat ~sep:":" 

  let of_string s : time =
    match String.split_on_chars ~on:[':'] s
        |>List.map ~f:Int.of_string with
    | [h; m; s] -> {h=h; m=m; s=s}
    | _         -> failwith "time_of_string: not a well-formed time."
end

type datetime = date * time

let current () =
  Date.current () ^ " " ^ Time.current ()

let to_json = function (d, t) ->
  `Assoc
  [ ("dd", d.dd)
  ; ("mm", d.mm)
  ; ("yyyy", d.yyyy)
  ; ("h", t.h)
  ; ("m", t.m)
  ; ("s", t.s)
  ]

let to_string = function (d, t) ->
  (Date.to_string d) ^ " " ^ (Time.to_string t)

let of_string s =
  match String.split_on_chars ~on:[' '] s with
  | [sd; st] -> (Date.of_string sd, Time.of_string st)
  | _ -> failwith "datetime_of_string: invalid datetime."
