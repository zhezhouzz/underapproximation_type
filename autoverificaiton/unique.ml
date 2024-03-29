let universe_label = ref 0

let name () =
  let n = Printf.sprintf "_x%i" !universe_label in
  universe_label := !universe_label + 1;
  n

let name_tab = Hashtbl.create 100

open Printf

let split_char = '!'

let unique name =
  match Hashtbl.find_opt name_tab name with
  | Some n ->
      Hashtbl.replace name_tab name (n + 1);
      sprintf "%s%c%i" name split_char (n + 1)
  | None ->
      Hashtbl.add name_tab name 0;
      sprintf "%s%c%i" name split_char 0
