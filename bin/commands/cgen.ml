open Sugar
open Zzdatatype.Datatype
open QCheck

type 'a t = Random.State.t -> 'a

let nat_gen : int t = Gen.small_nat

let list_gen_wrong : int list t =
 fun (r : Random.State.t) ->
  let len = nat_gen r in
  let elem = nat_gen r in
  let rec aux (n : int) : int list =
    if n == 0 then [] else elem :: aux (n - 1)
  in
  aux len

let list_gen_correct : int list t =
 fun (r : Random.State.t) ->
  let len = nat_gen r in
  let rec aux (n : int) : int list =
    if n == 0 then [] else nat_gen r :: aux (n - 1)
  in
  aux len

let test (num : int) =
  let ls1 = Gen.generate ~n:num list_gen_wrong in
  let () = Printf.printf "list_gen_wrong:\n" in
  let () =
    List.iter (fun l -> Printf.printf "[%s]\n" (IntList.to_string l)) ls1
  in
  let () = Printf.printf "\nlist_gen_correct:\n" in
  let ls2 = Gen.generate ~n:num list_gen_correct in
  let () =
    List.iter (fun l -> Printf.printf "[%s]\n" (IntList.to_string l)) ls2
  in
  ()
