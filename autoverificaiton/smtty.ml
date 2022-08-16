module T = struct
  type t = Bool | Int [@@deriving sexp]

  let smtty_eq = function Bool, Bool | Int, Int -> true | _ -> false
  let eq a b = smtty_eq (a, b)
  let layout = function Bool -> "B" | Int -> "I"

  let pretty_typed_layout str = function
    | Bool -> Printf.sprintf "(%s:𝓑 )" str
    | Int -> str
end
