val ra : int
val pa : int
val da : int
val w : int
val n_regs : int

module As : module type of Asm.Make(HardCaml.Signal.Comb)

module Phase : sig type t = If | D | E | Wb deriving(Enum,Bounded,Show) end

module Avalon_master_i : interface readdata waitrequest end
module Avalon_master_o : interface address read write writedata end
module Avalon_stream_i : interface data_i valid_i ready_i end
module Avalon_stream_o : interface data_o valid_o ready_o end

module Ttc_state : interface 
  phase pc rav rbv alu_result
  imem_re imem_we rf_we stall
end

module Ttc_imem : interface we d wa end

module Ttc_i : interface
  clk clr enable
  (avm : Avalon_master_i) 
  (ast : Avalon_stream_i)
end

module Ttc_o : interface
  (avm : Avalon_master_o) 
  (ast : Avalon_stream_o)
end

module Ttc_sim_i : interface
  (i : Ttc_i)
  (mem : Ttc_imem)
end

module Ttc_sim_o : interface
  (state : Ttc_state) 
  rf{| |}
  (instr : As.Instruction)
  (o : Ttc_o)
end

open HardCaml.Signal.Types

val ttc : signal Ttc_sim_i.t -> signal Ttc_sim_o.t
val ttc_top : signal Ttc_i.t -> signal Ttc_o.t

module Make(B : HardCaml.Comb.S) : sig

  module Ab : module type of Asm.Make(B)

  val sim : ?gtkwave : bool ->
            input:(int32 -> int32 -> int32) option -> output:(int32 -> unit) option ->
            im:Ab.Asm.assembled_program -> dm:int32 array -> 
            < step : unit; 
              reg : int -> int32; 
              pc : int; >

end

