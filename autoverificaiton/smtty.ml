module T = struct
  type t = Bool | Int [@@deriving sexp]

  let eq = function Bool, Bool | Int, Int -> true | _ -> false
  let layout = function Bool -> "B" | Int -> "I"
end
