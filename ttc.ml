(*****************************************************************************
 * Thacker's Tiny Computer 3 in HardCaml
 * (C) 2014 MicroJamJar Ltd.
 * ==============================================================
 * based on ttc.sv 
 * http://www.cl.cam.ac.uk/teaching/1314/ECAD+Arch/labs/background/ttc.html
 * Copyright Simon Moore, Frankie Robertson and Ben Thorner, 2012
 *****************************************************************************)

open HardCaml
open Signal.Comb
open Signal.Seq
open Signal.Guarded

(****************************************************************************)

(* vector sizes *)
let ra = 7
let pa = 10
let da = 10
let w = 32
let n_regs = 1 lsl ra

let invalid_data = consthu w "deadbeef"

(* enumeration types *)
module As = Asm.Make(Signal.Comb)
open As.Instruction

module Func' = Deriving_bits.Make(Func)(Signal.Comb)
open Func

module Rotate' = Deriving_bits.Make(Rotate)(Signal.Comb)
open Rotate

module Skip' = Deriving_bits.Make(Skip)(Signal.Comb)
open Skip

module Op' = Deriving_bits.Make(Op)(Signal.Comb)
open Op

module Phase = struct type t = If | D | E | Wb deriving(Enum,Bounded,Show) end
module Phase' = Deriving_bits.Make(Phase)(Signal.Comb)
open Phase

(****************************************************************************)

let mux' sel default l = 
  let max = 1 + List.fold_left (fun acc (i,_) -> max i acc) 0 l in
  let a = Array.create max default in
  let () = List.iter (fun (i,x) -> a.(i) <- x) l in
  if 1 lsl (width sel) = max then
    mux sel (Array.to_list a)
  else
    mux sel (Array.to_list a @ [default])

let pc_mux instr alu pc avm_m1_waitrequest asi_in_valid = 
  let pc1, pc2 = pc +:. 1, pc +:. 2 in
  let cond = (~: avm_m1_waitrequest) &: (~: (Op'.(instr.op ==: In) &: (~: asi_in_valid))) in
  mux2 instr.lc pc1
    (mux2 Op'.(instr.op ==: Jmp) (uresize alu pa)
      (Skip'.case instr.skip pc1
        [
          Ltz, mux2 (alu <+. 0) pc2 pc1;
          Eqz, mux2 (alu ==:. 0) pc2 pc1;
          Inr, mux2 cond pc2 pc1;
        ]))

let result_mux instr pc alu dm_result in_result = 
  mux2 instr.lc 
    (uresize instr.const w) 
    (Op'.case instr.op alu
      [
        Ldd, dm_result;
        In, in_result;
        Jmp, uresize (pc +:. 1) w;
      ])

let alu instr ra rb = 
  let pre_shift = 
    Func'.case instr.func invalid_data
      [
        Add,   (ra +: rb);
        Sub,   (ra -: rb);
        Inc,   (ra +:. 1);
        Dec,   (ra -:. 1);
        And,   (ra &: rb);
        Or,    (ra |: rb);
        Xor,   (ra ^: rb);
        Fpmul, invalid_data
     ]
  in
  let rot x n = select x (n-1) 0 @: select x (width x - 1) n in
  Rotate'.case instr.rotate pre_shift
    [
      Rcy1,  rot pre_shift 1;
      Rcy8,  rot pre_shift 8;
      Rcy16, rot pre_shift 16;
    ]

let stall_mux instr avm_m1_waitrequest aso_out_valid asi_in_valid = 
  mux2 instr.lc gnd
    (Op'.case instr.op gnd
      [
        Out, aso_out_valid;
        In, ~: asi_in_valid;
        Std, avm_m1_waitrequest;
        Ldd, avm_m1_waitrequest;
      ])

type regfile = Signal.Types.register -> Signal.Types.signal -> 
               Signal.Types.signal -> Signal.Types.signal -> Signal.Types.signal -> 
               Signal.Types.signal -> 
               (Signal.Types.signal * Signal.Types.signal * Signal.Types.signal array)

let regfile_r r_spec enable ra rb w d =
  let we = binary_to_onehot w in
  let rf = Array.init n_regs (fun i -> reg r_spec (enable &: bit we i) d) in
  let q = Array.to_list rf in
  mux ra q, mux rb q, rf

let regfile_m r_spec enable ra rb w d = 
  memory ~size:n_regs ~spec:r_spec ~w:w ~we:enable ~d ~r:ra,
  memory ~size:n_regs ~spec:r_spec ~w:w ~we:enable ~d ~r:rb,
  [||] (* cannot provide visibility into the registers here *)

module Avalon_master_i = interface readdata[w] waitrequest[1] end
module Avalon_master_o = interface address[da] read[1] write[1] writedata[w] end
module Avalon_stream_i = interface data_i[w] valid_i[1] ready_i[1] end
module Avalon_stream_o = interface data_o[w] valid_o[1] ready_o[1] end

module Ttc_imem = interface we[1] d[w] wa[pa] end

module Ttc_state = interface 
  phase[2] pc[pa] rav[w] rbv[w] alu_result[w]
  imem_re[1] imem_we[1] rf_we[1] stall[1]
end

module Ttc_i = interface
  clk[1] clr[1] enable[1]
  (avm : Avalon_master_i) 
  (ast : Avalon_stream_i)
end

module Ttc_o = interface
  (avm : Avalon_master_o) 
  (ast : Avalon_stream_o)
end

module Ttc_sim_i = interface
  (i : Ttc_i)
  (mem : Ttc_imem)
end

module Ttc_sim_o = interface
  (state : Ttc_state) rf{|n_regs|}[w]
  (instr : As.Instruction)
  (o : Ttc_o)
end

let imem ~cfg ~size ~spec ~we ~wa ~d ~re ~ra =
  let open Ttc_imem in
  if empty = cfg.we then 
    ram_rbw ~size ~spec ~we ~wa ~d ~re ~ra
  else 
    let we = we |: cfg.we in
    let wa = mux2 cfg.we cfg.wa wa in
    let d = mux2 cfg.we cfg.d d in
    ram_rbw ~size ~spec ~we ~wa ~d ~re ~ra

let ttc i = 
  let open Ttc_sim_i in
  let open Ttc_i in
  let open Avalon_master_i in
  let open Avalon_master_o in
  let open Avalon_stream_i in
  let open Avalon_stream_o in
  let open Ttc_state in
  let open Signal.Types in

  let m = i.mem in
  let i = i.i in

  let r_sync = { r_sync with reg_clock = i.clk; reg_clear = i.clr } in
  let reset_pending = reg r_sync enable i.avm.waitrequest in

  let r_sync = { r_sync with reg_clock = i.clk; reg_clear = i.clr |: reset_pending } in
  let r_none = { r_none with Signal.Types.reg_clock = i.clk } in

  let st = Ttc_state.map (fun (n,b) ->
    match n with
    | "imem_re" | "imem_we" | "rf_we" | "stall" -> g_wire gnd
    | _ -> g_reg r_sync enable b) Ttc_state.t 
  in

  let avm = Avalon_master_o.(map (fun (_,b) -> g_reg r_sync enable b) t) in
  let ast = Avalon_stream_o.(map (fun (_,b) -> g_reg r_sync enable b) t) in

  (* instruction memory *)
  let imem_q = imem ~cfg:m ~size:(1 lsl pa) ~spec:r_none 
    ~we:st.imem_we#q ~wa:(uresize st.rbv#q pa) ~d:st.rav#q
    ~re:st.imem_re#q ~ra:st.pc#q
  in
  let instr = decode imem_q in

  (* register file *)
  let _rav, _rbv, rf = regfile_r r_none (enable &: st.rf_we#q) instr.ra instr.rb instr.rw 
    (result_mux instr st.pc#q st.alu_result#q i.avm.readdata i.ast.data_i)
  in

  (* 4 stage sequential execution *)
  let () = compile [
    g_when Phase'.(st.phase#q ==: If) [
      st.imem_re $== vdd;
      st.phase $== Phase'.to_bits D;
    ];

    g_when Phase'.(st.phase#q ==: D) [
      st.rav $== _rav;
      st.rbv $== _rbv;
      st.phase $== Phase'.to_bits E;
    ];

    g_when Phase'.(st.phase#q ==: E) [
      st.alu_result $== (alu instr st.rav#q st.rbv#q);
      g_when (~: (instr.lc)) [
        g_switch (instr.op) [
          Op'.to_bits Ldd, [
            avm.read $== vdd;
            avm.address $== uresize st.rbv#q da;
          ];
          Op'.to_bits Std, [
            avm.write $== vdd;
            avm.address $== uresize st.rbv#q da;
            avm.writedata $== st.rav#q;
          ];
        ];
      ];
      st.phase $== Phase'.to_bits Wb;
    ];

    st.stall $== stall_mux instr i.avm.waitrequest ast.valid_o#q i.ast.valid_i;
    g_when Phase'.(st.phase#q ==: Wb) [
      g_unless st.stall#q [
        st.rf_we $== vdd;
        st.pc $== (pc_mux instr st.alu_result#q st.pc#q i.avm.waitrequest i.ast.valid_i);
        g_unless (instr.lc) [
          g_switch (instr.op) [
            Op'.to_bits Out, [
              ast.valid_o $== vdd;
              ast.data_o $== st.rav#q;
            ];
            Op'.to_bits In,  [ ast.ready_o $== vdd; ];
            Op'.to_bits Ldd, [ avm.read $== gnd ];
            Op'.to_bits Std, [ avm.write $== gnd ];
            Op'.to_bits Sti, [ st.imem_we $== vdd ];
          ];
        ];
        st.phase $== Phase'.to_bits If;
      ];

      g_when (i.ast.ready_i &: ast.valid_o#q) [
        ast.valid_o $== gnd;
      ];

      g_when (ast.ready_o#q &: ~: (i.ast.valid_i)) [
        ast.ready_o $== gnd;
      ];

    ];

  ] in

  Ttc_sim_o.({
    state = Ttc_state.map (fun d -> d#q) st;
    rf = rf;
    instr = instr;
    o = Ttc_o.({
      avm = Avalon_master_o.map (fun d -> d#q) avm;
      ast = Avalon_stream_o.map (fun d -> d#q) ast;
    });
  })

let ttc_top i =
  (ttc Ttc_sim_i.({
    i = i;
    mem = Ttc_imem.(map (fun _ -> empty) t);
  })).Ttc_sim_o.o 

(****************************************************************************)

module Make(B : Comb.S) = struct

  module Ab = Asm.Make(B)

  let sim ?(gtkwave=false) ~input ~output ~im ~dm = 
    let module Builder = Interface.Gen(B)(Ttc_sim_i)(Ttc_sim_o) in
    let module S = Cyclesim.Api in
    let module Vcd = Vcd_ext.Make(B) in

    let open Ttc_sim_i in
    let open Ttc_sim_o in
    let open Ttc_state in
    let open Ttc_imem in
    let open Ttc_i in
    let open Ttc_o in

    let circ,sim,i,o = Builder.make "ttc" ttc in
    let sim = if gtkwave then Vcd.gtkwave ~args:"-S gwShowall.tcl" sim else sim in

    (* reset core and load program *)
    let Ab.Asm.Assembled im = im in
    S.reset sim;
    i.i.clr := B.vdd;
    for j=0 to Array.length im - 1 do
      i.mem.we := B.vdd;
      i.mem.wa := B.consti pa j;
      i.mem.d := im.(j);
      S.cycle sim;
    done;
    i.mem.we := B.vdd;

    (* set up busses *)
    i.i.ast.Avalon_stream_i.valid_i := B.vdd;
    i.i.ast.Avalon_stream_i.data_i := B.consti w 10;

    (* release reset, enable and run core *)
    i.i.clr := B.gnd;
    i.i.enable := B.vdd;

    let step () = 
      (* 4 cycles per instuction *)
      S.cycle sim;
      S.cycle sim;
      S.cycle sim;
      S.cycle sim;
      S.cycle_comb sim;
    in

    object
      method step = step ()
      method pc = B.to_int !(o.state.pc)
      method reg i = B.to_int32 !(o.rf.(i))
    end

end

