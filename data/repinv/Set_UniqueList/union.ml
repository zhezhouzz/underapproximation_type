(* let rec union (s1 : elem list) (s2 : elem list) : elem list = *)
(*   match s1 with *)
(*   | [] -> s2 *)
(*   | h1 :: t1 -> ( *)
(*       match s2 with *)
(*       | [] -> s1 *)
(*       | h2 :: t2 -> *)
(*           if h1 == h2 then union t1 s2 *)
(*           else if h1 < h2 then h1 :: union t1 s2 *)
(*           else h2 :: union s1 t2) *)
let rec union (s1 : elem list) (s2 : elem list) : elem list =
  match s1 with
  | [] -> s2
  | h1 :: t1 -> (
      match s2 with
      | [] -> Err
      | h2 :: t2 ->
          if elem_eq h1 h2 then Err
          else if elem_lt h1 h2 then h1 :: union t1 s2
          else Err)

let[@assert] union =
  [|
    (let (i [@ghost]) = (true : [%v: int]) [@over] in
     let s1 = (emp v : [%v: elem list]) [@over] in
     let s2 = (sorted v && lenlte v i : [%v: elem list]) [@under] in
     (sorted v && len v i : [%v: elem list]) [@under]);
    (let (n [@ghost]) = (2 <= v : [%v: int]) [@over] in
     let s1 = (sorted v && lenlte v (n - 1) : [%v: elem list]) [@under] in
     let s2 = (sorted v && lenlte v (n - 1) : [%v: elem list]) [@under] in
     (sorted v && len v n : [%v: elem list]) [@under]);
  |]
