val f : int -> int

let[@library] f =
  let b = (true : [%v: int]) in
  (true : [%v: int])

let double_app (a : int) (b : int) : int = f a + f b

let[@assert] double_app ?l:(a = (0 <= v : [%v: int])) =
  let b = (true : [%v: int]) in
  (true : [%v: int])
