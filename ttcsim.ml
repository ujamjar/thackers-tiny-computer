open HardCaml
open Ttc
module B = Bits.Comb.IntbitsList
module A = Asm.Make(B)
module S = Ttc.Make(B)

let cycles = ref 0
let gtkwave = ref false
let program = ref ""
let interp = ref false
let verbose = ref false

let () = Arg.parse 
  [
    "-c", Arg.Set_int cycles, "number of machine cycles to run";
    "-w", Arg.Set gtkwave, "launch gtkwave";
    "-i", Arg.Set interp, "run interpreter instead of simulator";
    "-v", Arg.Set verbose, "verbose";
  ]
  (fun s -> program := s)
  "Thackers Tiny Computer Program Simulator"

let rec read_lines f = 
  match try Some (input_line f |> B.const) with _ -> None with
  | None -> []
  | Some(x) -> x :: read_lines f

let program = 
  match !program with
  | "" -> read_lines stdin |> Array.of_list
  | x -> 
      let f = open_in x in
      let p = read_lines f in
      let () = close_in f in
      p |> Array.of_list

let output x = 
  Printf.printf "OUTPUT: 0x%.8lx\n%!" x

let input _ _ = 
  Printf.printf "INPUT: %!";
  input_line stdin |> Int32.of_string

let cpu = 
  if !interp then 
    A.Interpreter.make 
      ~start_address:0
      ~input:(Some input) ~output:(Some output)
      ~im:(A.Asm.Assembled(program))
      ~dm:[||]
  else
    S.sim
      ~gtkwave:(!gtkwave)
      ~input:(Some input) ~output:(Some output)
      ~im:(A.Asm.Assembled(program))
      ~dm:[||]

let () = 
  for i=0 to !cycles - 1 do
    if !verbose then begin
      Printf.printf "PC: %i [%s]\n%!" cpu#pc 
        (try A.Printer.string_of_instr program.(cpu#pc) with _ -> "-");
    end;
    cpu#step
  done


