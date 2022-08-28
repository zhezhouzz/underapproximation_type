module T = struct
  type t = Bool | Int | Dt [@@deriving sexp]

  let smtty_eq = function
    | Bool, Bool | Int, Int -> true
    | Dt, Dt -> true
    | _ -> false

  let eq a b = smtty_eq (a, b)
  let layout = function Bool -> "B" | Int -> "I" | Dt -> "D"

  let pretty_typed_layout str = function
    | Bool -> Printf.sprintf "(%s:𝓑 )" str
    | Dt -> Printf.sprintf "(%s:𝓓 )" str
    | Int -> str

  let is_dt = function Dt -> true | _ -> false
end
