let rec stream_gen (size : int) : int stream =
  if size == 0 then Streamnil
  else if bool_gen () then Err
  else
    let (l : int stream) = stream_gen (size - 1) in
    Streamlazycons (int_gen (), Lazyty l)

let[@assert] stream_gen =
  let s = (0 <= v : [%v: int]) [@over] in
  (fun ((u [@exists]) : int) -> stream_len v u && u <= s
    : [%v: int stream])
    [@under]
