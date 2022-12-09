let rec run code tape_pos tape =
  let move_right =
    let len = Array.length tape in
    if tape_pos = len - 1
    then (tape_pos, Array.append tape [|0|])
    else (tape_pos + 1, tape)
  in
  let move_left =
    if tape_pos = 0
    then (tape_pos, tape)
    else (tape_pos - 1, tape)
  in
  let incr = (tape_pos, Array.mapi (fun i x -> if i = tape_pos then x + 1 else x) tape) in
  let decr = (tape_pos, Array.mapi (fun i x -> if i = tape_pos then x - 1 else x) tape) in
  match code with
  | [] -> ()
  | c :: rest ->
    let (new_pos, new_tape) =
      match c with
      | '>' -> move_right
      | '<' -> move_left
      | '+' -> incr
      | '-' -> decr
      | _ -> (tape_pos, tape)
    in
    run rest new_pos new_tape
