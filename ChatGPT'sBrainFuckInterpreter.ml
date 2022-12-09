(* Can you write a Brainfuck interpreter with OCaml? *)

(* Yes, it is possible to write a Brainfuck interpreter using OCaml.
   Brainfuck is an esoteric programming language that uses a very minimalistic instruction set,
   so the interpreter would not be very complex.
   Here is an example of how the interpreter could be implemented in OCaml: *)

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


(* This interpreter uses a simple functional approach,
   with a recursive run function that takes the Brainfuck code as a string,
   the current position on the tape, and the tape itself
   (represented as an array of integers).
   The function handles each instruction in the code by updating the tape and tape position as necessary,
   and then calls itself recursively with the updated state to process the next instruction.

   The interpreter handles the > and < instructions by moving the tape position to the right or left, respectively.
   The + and - instructions are handled by incrementing or decrementing the value at the current position on the tape.
   Any other characters in the code are ignored.

   I hope this helps! Let me know if you have any other questions. *)
