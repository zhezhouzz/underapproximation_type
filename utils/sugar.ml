let spf = Printf.sprintf

let make_dir name = Core.Unix.mkdir_p name

let rec fastexpt : int -> int -> int =
 fun b n ->
  if n = 0 then 1
  else
    let b2 = fastexpt b (n / 2) in
    if n mod 2 = 0 then b2 * b2 else b * b2 * b2

let map2 f (a, b) = (f a, f b)

let map3 f (a, b, c) = (f a, f b, f c)

let map4 f (a, b, c, d) = (f a, f b, f c, f d)

let map5 f (a, b, c, d, e) = (f a, f b, f c, f d, f e)

let map6 f (a, b, c, d, e, g) = (f a, f b, f c, f d, f e, f g)

let map7 f (a, b, c, d, e, g, h) = (f a, f b, f c, f d, f e, f g, f h)

let opt_comapre c x y =
  match (x, y) with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some x, Some y -> c x y

let opt_fmap (x : 'a option) (f : 'a -> 'b) : 'b option =
  match x with None -> None | Some x -> Some (f x)

let opt_bind (x : 'a option) (f : 'a -> 'b option) : 'b option =
  match x with None -> None | Some x -> f x

let bopt_false = function Some b -> b | None -> false

let opt_list_to_list_opt l =
  List.fold_right
    (fun x l ->
      match (x, l) with
      | None, _ -> None
      | _, None -> None
      | Some x, Some l -> Some (x :: l))
    l (Some [])

let ( let* ) x f = opt_bind x f

let ( let+ ) x f = opt_fmap x f

let compare_bind a b = if a != 0 then a else b
