(* let rec union (s1 : int list) (s2 : int list) : int list = *)
(*   match s1 with *)
(*   | [] -> s2 *)
(*   | h1 :: t1 -> ( *)
(*       match s2 with *)
(*       | [] -> s1 *)
(*       | h2 :: t2 -> *)
(*           if h1 == h2 then union t1 s2 *)
(*           else if h1 < h2 then h1 :: union t1 s2 *)
(*           else h2 :: union s1 t2) *)

let rec union (s1 : int list) (s2 : int list) : int list =
  match s1 with
  | [] -> Err
  | h1 :: t1 -> (
      match s2 with
      | [] -> Err
      | h2 :: t2 ->
          if h1 == h2 then Err else if h1 < h2 then h1 :: union t1 s2 else Err)

let[@assert] union =
  let (i [@ghost]) = (1 <= v : [%v: int]) [@over] in
  let s1 = (sorted v && lenlte v i : [%v: int list]) [@under] in
  let s2 = (sorted v && lenlte v i : [%v: int list]) [@under] in
  (sorted v && len v i : [%v: int list]) [@under]
