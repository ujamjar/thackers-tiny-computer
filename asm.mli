
module Make(B : HardCaml.Comb.S) : sig

  module Instruction : sig
    module Func : sig
      type t = Add | Sub | Inc | Dec | And | Or | Xor | Fpmul deriving(Enum,Bounded,Show) 
    end
    module Rotate : sig type t = Nsh | Rcy1 | Rcy8 | Rcy16 deriving(Enum,Bounded,Show) end
    module Skip : sig type t = Nsk | Ltz | Eqz | Inr deriving(Enum,Bounded,Show) end
    module Op : sig type t = Fun | Std | Sti | Out | Ldd | In | Jmp deriving(Enum,Bounded,Show) end
    module T : interface rw lc ra rb func rotate skip op const end
    include module type of T
    val ranges : (int*int) t
    val decode : B.t -> B.t t
  end

  module Asm : sig
  
    open Instruction

    type instr = B.t

    val mk_instr : ?rot:Rotate.t -> ?skip:Skip.t -> ?op:Op.t -> ?func:Func.t -> 
      int -> int -> int -> instr
    val mk_const : int -> int -> instr

    type asm_instr = 
      | Instr of instr
      | Const of instr
      | ConstLabel of int * string
      | Block of asm_instr list

    val instr : ?rot:Rotate.t -> ?skip:Skip.t -> ?op:Op.t -> ?func:Func.t -> 
      int -> int -> int -> asm_instr
    val const : int -> int -> asm_instr
    val const_label : int -> string -> asm_instr
    val block : asm_instr list -> asm_instr
    
    val (<--) : int -> (int -> asm_instr) -> asm_instr
    val (<-.) : int -> int -> asm_instr
    val (<-:) : int -> string -> asm_instr

    val (+:) : ?rot:Rotate.t -> ?skip:Skip.t -> ?op:Op.t ->  int -> int -> (int -> asm_instr)
    val (-:) : ?rot:Rotate.t -> ?skip:Skip.t -> ?op:Op.t ->  int -> int -> (int -> asm_instr)
    val (&:) : ?rot:Rotate.t -> ?skip:Skip.t -> ?op:Op.t ->  int -> int -> (int -> asm_instr)
    val (|:) : ?rot:Rotate.t -> ?skip:Skip.t -> ?op:Op.t ->  int -> int -> (int -> asm_instr)
    val (^:) : ?rot:Rotate.t -> ?skip:Skip.t -> ?op:Op.t ->  int -> int -> (int -> asm_instr)
    val inc : ?rot:Rotate.t -> ?skip:Skip.t -> ?op:Op.t ->  int -> (int -> asm_instr)
    val dec : ?rot:Rotate.t -> ?skip:Skip.t -> ?op:Op.t ->  int -> (int -> asm_instr)
    val jmp : ?rot:Rotate.t -> ?func:Func.t ->  int -> int -> (int -> asm_instr)
    val jmpl : string -> (int -> asm_instr)
    val bnez : int -> int -> (int -> asm_instr)
    val bgez : int -> int -> (int -> asm_instr)

    type program = (string * asm_instr list) list
    type assembled_program = Assembled of instr array

    val assemble : program -> assembled_program
    val disassemble : assembled_program -> program

  end

  module Interpreter : sig

    val make : start_address:int -> 
               input:(int32 -> int32 -> int32) option -> output:(int32 -> unit) option ->
               im:Asm.assembled_program -> dm:int32 array -> 
               < step : unit; 
                 reg : int -> int32; 
                 pc : int; >

  end

  module Parser : sig

    type ('a,'b) pp = ('a, 'b) MParser.t
    type 'a p = ('a, unit) pp

    module Token : sig

      val explode : string -> char list
      val implode : char list -> string

      val whitespace : unit p
      val lexeme : 'a p -> 'a p
      val string : string -> string p
      val ident : string p
      val char : char -> char p
      val integer : int p

    end

    val assembly : Asm.program p

    val parse_string : string -> Asm.program option

    val parse_channel : ?verbose:bool -> in_channel -> Asm.program option

    val with_file : string -> (in_channel -> 'a) -> 'a

  end

  module Printer : sig

    val string_of_instr : B.t -> string

    val pretty : (string -> unit) -> Asm.program -> unit

    val td_of_instr : ?binary:bool -> ?map:(string -> string) -> B.t Instruction.T.t -> string
    val td_of_asm_instr : ?binary:bool -> ?map:(string -> string) -> Asm.asm_instr -> string list

    val html_of_asm_instr : ?binary:bool -> ?map:(string -> string) -> Asm.asm_instr -> string
    val html_of_program : ?binary:bool -> ?map:(string -> string) -> Asm.program -> string
    val html_of_assembled_program : ?binary:bool -> ?map:(string -> string) -> 
      Asm.assembled_program -> string

    val html_of_interpreter : Asm.assembled_program ->
      < reg : int -> int32; 
        pc : int; .. > -> 
        string

  end

end


