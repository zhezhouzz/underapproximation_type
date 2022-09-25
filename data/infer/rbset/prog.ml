let balance (color : bool) (left : int rbset) (elem : int) (right : int rbset) :
    int rbset =
  let (r : bool) = true in
  let (b : bool) = false in
  let (res : int rbset) = Rbsnode (color, left, elem, right) in
  let (res0 : int rbset) =
    match right with
    | Rbsnode (color4, left4, elem4, right4) ->
        if color4 then
          match left4 with
          | Rbsnode (color5, left5, elem5, right5) ->
              if color5 then
                Rbsnode
                  ( r,
                    Rbsnode (b, left, elem, left5),
                    elem5,
                    Rbsnode (b, right5, elem4, right4) )
              else res
          | Rbsleaf -> (
              match right4 with
              | Rbsnode (color6, left6, elem6, right6) ->
                  if color6 then
                    Rbsnode
                      ( r,
                        Rbsnode (b, left, elem, left4),
                        elem4,
                        Rbsnode (b, left6, elem6, right6) )
                  else res)
        else res
    | Rbsleaf -> res
  in
  match left with
  | Rbsnode (color1, left1, elem1, right1) ->
      if color1 then
        match left1 with
        | Rbsnode (color2, left2, elem2, right2) ->
            if color2 then
              Rbsnode
                ( r,
                  Rbsnode (b, left2, elem2, right2),
                  elem1,
                  Rbsnode (b, right1, elem, right) )
            else res0
        | Rbsleaf -> (
            match right1 with
            | Rbsnode (color3, left3, elem3, right3) ->
                if color3 then
                  Rbsnode
                    ( r,
                      Rbsnode (b, left1, elem1, left3),
                      elem3,
                      Rbsnode (b, right3, elem, right) )
                else res0)
      else res0
  | Rbsleaf -> res
