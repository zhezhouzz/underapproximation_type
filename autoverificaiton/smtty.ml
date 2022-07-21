module T = struct
  type t = Bool | Int [@@deriving sexp]

  let eq = function Bool, Bool | Int, Int -> true | _ -> false
end
