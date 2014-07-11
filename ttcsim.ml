open HardCaml
open Ttc
module B = Bits.Comb.IntbitsList
module A = Asm.Make(B)

open A.Instruction.Func

let program : A.Asm.program =
  let open A.Asm in
  [
    "start", [
      const 0 10;
      const 1 20;
      const_label 3 "loop";
      instr ~func:Add 2 0 1;
      block [
        instr ~func:Sub 3 2 1;
        instr ~func:Xor 3 2 1;
      ];
    ];
    "loop", [
    ];
  ]
(*
let testbench program = 
  let () = Printf.printf "Thacker's Tiny 3 Computer Simulation\n" in

  let module Builder = Interface.Gen(B)(Ttc_i)(Ttc_o_debug) in
  let module S = Cyclesim.Api in
  let module Vcd = Vcd_ext.Make(B) in

  let open Ttc_i in
  let open Ttc_o_debug in

  let circ,sim,i,o = Builder.make "ttc" ttc_debug in
  let sim = Vcd.gtkwave ~args:"-S gwShowall.tcl" sim in

  let cycle() = S.cycle sim; S.cycle_comb sim in

  (* reset *)
  S.reset sim;
  i.clr := B.vdd;
  cycle();
  i.clr := B.gnd;

  Printf.printf "press return to continue\n%!";
  input_line stdin

(*let _ = testbench ()*)
*)
