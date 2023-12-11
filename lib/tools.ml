open Base

let get_file_content path =
  let ic = open_in path in
  let s = In_channel.input_all ic in
  close_in ic;
  s
