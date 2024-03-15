val hidden_list_gen : unit -> int list

let[@library] hidden_list_gen =
  let _ = (true : [%v: unit]) [@over] in
  (true : [%v: int list]) [@under]

let rec sized_list_gen (s : int) : int list =
  if s == 0 then Exn else if bool_gen () then hidden_list_gen () else Exn

let[@assert] sized_list_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (fun ((n [@exists]) : int) -> len v n && n <= s : [%v: int list]) [@under]
