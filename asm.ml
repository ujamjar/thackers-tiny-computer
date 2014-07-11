module Make(B : HardCaml.Comb.S) = struct

  module Instruction = struct
    module Func = struct 
      type t = Add | Sub | Inc | Dec | And | Or | Xor | Fpmul deriving(Enum,Bounded,Show) 
    end
    module Rotate = struct type t = Nsh | Rcy1 | Rcy8 | Rcy16 deriving(Enum,Bounded,Show) end
    module Skip = struct type t = Nsk | Ltz | Eqz | Inr deriving(Enum,Bounded,Show) end
    module Op = struct type t = Fun | Std | Sti | Out | Ldd | In | Jmp deriving(Enum,Bounded,Show) end

    module T = interface rw lc ra rb func rotate skip op const end
    include T

    let ranges = 
      {
        rw = (31,25); 
        lc = (24,24); 
        ra = (23,17); 
        rb = (16,10);
        func = (9,7); 
        rotate = (6,5); 
        skip = (4,3); 
        op = (2,0);
        const = (23,0);
      }

    let decode i = map (fun (h,l) -> B.select i h l) ranges

  end

  module Asm = struct

    open Instruction

    type instr = B.t

    let mk_instr ?rot ?skip ?op ?func rw ra rb = 
      let rot = 
        match rot with
        | None   -> Rotate.(Enum.from_enum<t> Nsh)
        | Some x -> Rotate.(Enum.from_enum<t> x)
      in
      
      let skip = 
        match skip with
        | None   -> Skip.(Enum.from_enum<t> Nsk)
        | Some x -> Skip.(Enum.from_enum<t> x)
      in

      let op = 
        match op with
        | None   -> Op.(Enum.from_enum<t> Fun)
        | Some x -> Op.(Enum.from_enum<t> x)
      in

      let func = 
        match func with
        | None   -> Func.(Enum.from_enum<t> And)
        | Some x -> Func.(Enum.from_enum<t> x)
      in
      [ 7,rw; 1,0; 7,ra; 7,rb; 3,func; 2,rot; 2,skip; 3,op ] 
        |> List.map (fun (b,x) -> B.consti b x) |> B.concat

    let mk_const w x = B.concat [ B.consti 7 w; B.vdd; B.consti 24 x ]

    type asm_instr = 
      | Instr of instr
      | Const of instr
      | ConstLabel of int * string
      | Block of asm_instr list

    let instr ?rot ?skip ?op ?func rw ra rb = 
      Instr (mk_instr ?rot ?skip ?op ?func rw ra rb)
    let const w x = Const (mk_const w x)
    let const_label w x = ConstLabel (w, x)
    let block x = Block x

    let (<--) rw f = f rw
    let (<-.) rw i = const rw i
    let (<-:) rw l = const_label rw l

    let (+:) ?rot ?skip ?op ra rb = fun rw -> instr ?rot ?skip ?op ~func:Func.Add rw ra rb
    let (-:) ?rot ?skip ?op ra rb = fun rw -> instr ?rot ?skip ?op ~func:Func.Sub rw ra rb
    let (&:) ?rot ?skip ?op ra rb = fun rw -> instr ?rot ?skip ?op ~func:Func.And rw ra rb
    let (|:) ?rot ?skip ?op ra rb = fun rw -> instr ?rot ?skip ?op ~func:Func.Or rw ra rb
    let (^:) ?rot ?skip ?op ra rb = fun rw -> instr ?rot ?skip ?op ~func:Func.Xor rw ra rb

    let inc ?rot ?skip ?op ra = fun rw -> instr ?rot ?skip ?op ~func:Func.Inc rw ra ra
    let dec ?rot ?skip ?op ra = fun rw -> instr ?rot ?skip ?op ~func:Func.Dec rw ra ra

    let jmp ?rot ?func ra rb = fun rw -> instr ?rot ?func ~op:Op.Jmp rw ra rb
    let jmpl label = fun rw -> 
      block [
        rw <-: label;
        rw <-- jmp rw rw;
      ]

    let bnez ra rb = fun rw -> 
      block [
        ra <-- (&:) ra ra ~skip:Skip.Eqz;
        rw <-- jmp rb rb;
      ]
    let bgez ra rb = fun rw -> 
      block [
        ra <-- (&:) ra ra ~skip:Skip.Ltz;
        rw <-- jmp rb rb;
      ]

    type program = (string * asm_instr list) list
    type assembled_program = Assembled of instr array

    module M = Map.Make(String)

    let assemble program = 
      (* compute labels *)
      let rec f map n = function
        | [] -> map
        | (label,instrs)::t ->
            let rec g n = function
              | [] -> n
              | Block(h)::t -> g (g n h) t
              | h::t -> g (n+1) t
            in
            f (M.add label n map) (g n instrs) t
      in
      let labels = f M.empty 0 program in
      (* get instructions, compute label constants *)
      let rec f = function
        | [] -> []
        | (_,h)::t ->
            let find x = 
              try M.find x labels 
              with Not_found -> failwith ("Label not found: " ^ x)
            in
            let rec g = function
              | [] -> []
              | Block(h)::t -> g h @ g t
              | Instr i::t 
              | Const i::t -> i :: g t
              | ConstLabel(w,x)::t -> mk_const w (find x) :: g t
            in
            g h @ f t
      in
      Assembled (f program |> Array.of_list)

    let disassemble (Assembled i) = 
      let i = Array.map 
        (fun i -> 
          let i' = decode i in
          if B.to_int i'.lc <> 0 then Const i
          else Instr i) i
      in
      [ "start", Array.to_list i ]

  end

  module Interpreter = struct

    let make ~start_address ~input ~output ~im ~dm =
      let open Instruction in
      let Asm.Assembled im = im in
      let im = Array.map (fun instr -> map B.to_int (decode instr)) im in
      let regs = Array.init 128 (fun _ -> 0l) in
      let pc = ref start_address in

      let rec step () =
        let instr = im.(!pc) in
        let op = Enum.to_enum<Instruction.Op.t> instr.op in
        let rotate = Enum.to_enum<Instruction.Rotate.t> instr.rotate in
        let func = Enum.to_enum<Instruction.Func.t> instr.func in
        let skip = Enum.to_enum<Instruction.Skip.t> instr.skip in
        
        if instr.lc <> 0 then begin
          (* load constant *)
          regs.(instr.rw) <- Int32.of_int instr.const;
          pc := !pc + 1
        end else begin
          let a, b = regs.(instr.ra), regs.(instr.rb) in

          let alu =
            match func with
            | Func.Add -> Int32.add a b
            | Func.Sub -> Int32.sub a b
            | Func.Inc -> Int32.succ a
            | Func.Dec -> Int32.pred a
            | Func.And -> Int32.logand a b
            | Func.Or -> Int32.logor a b
            | Func.Xor -> Int32.logxor a b
            | Func.Fpmul -> 0l
          in

          let f = 
            let rot x y = Int32.(logor (shift_left x (32-y)) (shift_right_logical x y)) in
            match rotate with
            | Rotate.Nsh -> alu
            | Rotate.Rcy1 -> rot alu 1
            | Rotate.Rcy8 -> rot alu 8
            | Rotate.Rcy16 -> rot alu 16
          in

          let npc =
            if op = Op.Jmp then !pc + 1
            else
              match skip with
              | Skip.Ltz when f < 0l -> !pc + 2
              | Skip.Eqz when f = 0l -> !pc + 2
              | Skip.Inr -> !pc + 2 (* input always ready *)
              | _ -> !pc + 1
          in

          match op with
          | Op.Fun-> begin
            regs.(instr.rw) <- f;
            pc := npc
          end
          | Op.Std -> begin
            regs.(instr.rw) <- f;
            dm.(Int32.to_int b) <- a;
            pc := npc
          end
          | Op.Sti -> begin
            regs.(instr.rw) <- f;
            im.(Int32.to_int b) <- map B.to_int (decode (B.consti32 32 a));
            pc := npc
          end
          | Op.Out -> begin
            regs.(instr.rw) <- f;
            (match output with
            | Some(o) -> o f
            | None -> ());
            pc := npc
          end
          | Op.Ldd -> begin
            regs.(instr.rw) <- dm.(Int32.to_int b);
            pc := npc
          end
          | Op.In -> begin
            regs.(instr.rw) <- 
              (match input with
              | Some(i) -> i a b
              | None -> 0l);
            pc := npc
          end
          | Op.Jmp -> begin
            regs.(instr.rw) <- Int32.of_int (!pc + 1);
            pc := Int32.to_int f;
          end;

        end
      in

      object
        method step = step ()
        method reg i = regs.(i)
        method pc = !pc
      end

  end

  module Parser = struct

    open MParser
    open MParser_RE
    open Instruction

    type ('a,'b) pp = ('a, 'b) MParser.t
    type 'a p = ('a, unit) pp

    module Token = struct

      let explode str =
        let len = String.length str in 
        let rec f i = 
          if i < len then str.[i] :: f (i+1)
          else []
        in
        f 0

      let implode list = 
        let len = List.length list in
        let str = String.create len in
        let rec f i = function
          | [] -> ()
          | h::t -> str.[i] <- h; f (i+1) t
        in
        f 0 list;
        str

      let bom = char '\xef' >> char '\xbb' >> char '\xbf'

      let line_comment = '#'

      let oneline s =
          (attempt (char line_comment)
        >> skip_many (satisfy ((!=) '\n'))
        >> return ()) s

      let ignore_space s = (blank >> return ()) s

      let whitespace s =
        (skip_many (ignore_space <|> oneline <?> "")) s

      let lexeme p = (p >>= fun x -> (whitespace >> return x))

      let ident = lexeme (many1 (letter <|> digit <|> char '_') |>> implode)
      let string name = lexeme (string name)
      let char name = lexeme (char name)
      let integer = (lexeme (many1 digit)) >>= fun x -> return (int_of_string (implode x))

      let eol = lexeme (char '\n') >> return ()

    end

    let p_opt p =
          (attempt (p >>= fun p -> return (Some p))) <|> (return None)

    let label = 
          (Token.ident >>= fun id -> Token.char ':' >> return id)

    let string_opt strs = 
          p_opt ((choice (List.map Token.string strs)) >>= fun s -> return s)

    let string_assoc strs = 
        ((choice (List.map (fun (s,_) -> Token.string s) strs)) >>= 
          fun s -> return (List.assoc s strs))

    let string_assoc_opt strs = p_opt (string_assoc strs)

    let op = string_assoc_opt 
      Op.([ "ldin",In; "lddm",Ldd; "stout",Out; 
                "stdm",Std; "stim",Sti; "normal",Fun ])
    let rot = string_assoc_opt 
      Rotate.([ "rot16",Rcy16; "rot1",Rcy1; "rot8",Rcy8; "rot0",Nsh ])
    let skip = string_assoc_opt 
      Skip.([ "sez",Eqz; "sltz",Ltz; "noskip",Nsk; "sin",Inr ])

    let reg = char 'r' >> Token.integer
    let reg_opt = p_opt reg 

    let bin_func_regs =  
      string_assoc Func.(["add",Add; "sub",Sub; "and",And; 
                              "or",Or; "xor",Xor; "fpmul",Fpmul]) >>= fun func ->
      reg >>= fun rw ->
      reg >>= fun ra ->
      reg >>= fun rb ->
        return (func, rw, ra, rb)

    let un_func_regs =
      string_assoc Func.(["inc",Inc; "dec",Dec]) >>= fun func ->
      reg >>= fun rw ->
      reg_opt >>= fun ra -> 
        return (match ra with 
                | None -> func, rw, rw, rw
                | Some(ra) -> func, rw, ra, ra)

    let func_regs = (attempt bin_func_regs) <|> un_func_regs

    let asm_general = 
      op >>= fun op ->
      func_regs >>= fun (func,rw,ra,rb) -> 
      rot >>= fun rot ->
      skip >>= fun skip ->
        return Asm.(instr ?rot ?skip ?op ~func rw ra rb)

    let asm_jmp = 
      Token.string "jmp" >> 
      func_regs >>= fun (func,rw,ra,rb) -> 
      rot >>= fun rot -> return Asm.(instr ~op:Op.Jmp ?rot ~func rw ra rb)

    let asm_jmp2 = 
      Token.string "jmp" >> 
      reg >>= fun rw ->
      reg >>= fun ra ->
        return Asm.(instr ~op:Op.Jmp rw ra ra)

    let asm_rot = 
      Token.string "rot" >> 
      reg >>= fun rw ->
      reg_opt >>= fun ra ->
      rot >>= fun rot -> (* not optional *)
        match ra with
        | None -> return Asm.(instr ?rot rw rw rw)
        | Some(ra) -> return Asm.(instr ?rot rw ra ra)

    let asm_lc = 
      Token.string "lc" >> 
      reg >>= fun rw ->
      Token.integer >>= fun i ->
        return Asm.(const rw i)

    let asm_lc2 = 
      Token.string "lc" >> 
      reg >>= fun rw ->
      Token.ident >>= fun ident ->
        return Asm.(const_label rw ident)

    let asm_ldin = 
      Token.string "ldin" >> 
      reg >>= fun rw ->
      skip >>= fun skip ->
        return Asm.(instr ~op:Op.In ?skip rw 0 0)

    let asm_stout = 
      Token.string "stout" >> 
      reg >>= fun rw ->
        return Asm.(instr ~op:Op.Out rw rw rw)

    let asm_bnez = 
      Token.string "bnez" >> 
      reg >>= fun rw ->
      reg >>= fun ra ->
      reg >>= fun rb ->
        return Asm.(block [
          instr ~skip:Skip.Eqz ra ra ra;
          instr ~op:Op.Jmp rw rb rb
        ])

    let asm_bgez = 
      Token.string "bgez" >> 
      reg >>= fun rw ->
      reg >>= fun ra ->
      reg >>= fun rb ->
        return Asm.(block [
          instr ~skip:Skip.Ltz ra ra ra;
          instr ~op:Op.Jmp rw rb rb
        ])

    let asm_instr = 
          (attempt asm_general)
      <|> (attempt asm_jmp)
      <|> (attempt asm_jmp2)
      <|> (attempt asm_rot)
      <|> (attempt asm_lc)
      <|> (attempt asm_lc2)
      <|> (attempt asm_ldin)
      <|> (attempt asm_stout)
      <|> (attempt asm_bgez)
      <|> (attempt asm_bnez)

    type asm_label_instr = 
      | L of string
      | I of Asm.asm_instr

    let asm = 
          (attempt label >>= fun l -> (((attempt Token.eol) <|> Token.whitespace) >> return (L l)))
      <|> (asm_instr >>= fun i -> (Token.eol >> return (I i)))

    let asm_line = 
          (attempt Token.eol >> return None)
      <|> (asm >>= fun x -> return (Some x))

    (* let rec many_eof p st = 
      ((attempt p >>= fun d ->
          many_eof p >>= fun dt ->
            return (d::dt))
      <|> (eof >> return [])) st *)

    let rec unwrap = function
      | [] -> []
      | None::t -> unwrap t
      | Some(x)::t -> x :: unwrap t

    let rec read_frag frag = function
      | [] -> List.rev frag,[]
      | I i :: t -> read_frag (i::frag) t
      | x -> List.rev frag,x

    let rec make_program l = 
      match l with
      | [] -> []
      | I i :: _ -> 
          let frag,l = read_frag [] l in
          ("",frag) :: make_program l
      | L l :: t ->
          let frag,t = read_frag [] t in
          (l,frag) :: make_program t

    let assembly = 
      option Token.bom >> Token.whitespace >> 
      many1 asm_line >>= fun lines ->
      return (make_program (unwrap lines))

    (* parse a string *)
    let parse_string s = 
      let open Printf in
      match parse_string assembly s () with
      | Success(x) ->  Some(x)
      | Failed(x,_) -> printf "Error:\n%s\n" x; None

    (* parse a file *)
    let parse_channel ?(verbose=false) file = 
      let open Printf in
      match parse_channel assembly file () with
      | Success(x) -> Some(x)
      | Failed(x,_) -> begin
          (if verbose then printf "ERROR:\n %s\n" x);
          None
      end

    let with_file filename fn = 
      let f = open_in filename in
      let x = fn f in
      close_in f;
      x

  end

  module Printer = struct

    open Instruction

    module Func = Deriving_bits.Make(Func)(B)
    module Rotate = Deriving_bits.Make(Rotate)(B)
    module Skip = Deriving_bits.Make(Skip)(B)
    module Op = Deriving_bits.Make(Op)(B)

    let string_of_instr b = 
      let i = decode b in
      if B.to_int i.lc = 0 then
        Show.show<Instruction.Op.t> (Op.of_bits i.op) ^ " " ^
        Show.show<Instruction.Func.t> (Func.of_bits i.func) ^ " " ^
        "r" ^ string_of_int (B.to_int i.rw) ^ " " ^ 
        "r" ^ string_of_int (B.to_int i.ra) ^ " " ^ 
        "r" ^ string_of_int (B.to_int i.rb) ^ " " ^ 
        Show.show<Instruction.Rotate.t> (Rotate.of_bits i.rotate) ^ " " ^
        Show.show<Instruction.Skip.t> (Skip.of_bits i.skip) 
      else
        "lc " ^ 
        "r" ^ string_of_int (B.to_int i.rw) ^ " " ^ 
        string_of_int (B.to_int i.const)

    let pretty out prog = 
      List.iter (fun (l,p) ->
        if l<>"" then begin
          out l; out ":\n";
        end;
        let rec write_instrs = function
          | [] -> ()
          | (Asm.Instr i) :: t 
          | (Asm.Const i) :: t -> begin
            out "  "; 
            out (string_of_instr i);
            out "\n";
            write_instrs t
          end
          | Asm.ConstLabel(r,l) :: t -> begin
            out "  "; 
            out "Lc r"; out (string_of_int r); out " "; out l;
            out "\n";
            write_instrs t
          end
          | Asm.Block(b) :: t -> begin
            write_instrs b; 
            write_instrs t
          end
        in
        write_instrs p
      ) prog

    (* convert to html *)

    let tag ?style t d = 
      match style with
      | None -> "<" ^ t ^ ">" ^ d ^ "</" ^ t ^ ">\n" 
      | Some style -> "<" ^ t ^ " " ^ style ^ ">" ^ d ^ "</" ^ t ^ ">\n" 

    let td_of_instr ?(binary=false) ?(map=(fun s -> s)) i = 
      let td ?style x = tag ?style "td" (map x) in
      let instr_c i = 
        [
          "r" ^ string_of_int (B.to_int i.rw) |> td;
          "I" |> td;
          "r" ^ string_of_int (B.to_int i.ra) |> td;
          "r" ^ string_of_int (B.to_int i.rb) |> td;
          Show.show<Instruction.Func.t> (Func.of_bits i.func) |> td;
          Show.show<Instruction.Rotate.t> (Rotate.of_bits i.rotate) |> td;
          Show.show<Instruction.Skip.t> (Skip.of_bits i.skip) |> td;
          Show.show<Instruction.Op.t> (Op.of_bits i.op) |> td;
        ] |> String.concat ""
      in
      let instr_b i = 
        [
          B.to_string i.rw |> td;
          "0" |> td;
          B.to_string i.ra |> td;
          B.to_string i.rb |> td;
          B.to_string i.func |> td;
          B.to_string i.rotate |> td;
          B.to_string i.skip |> td;
          B.to_string i.op |> td;
        ] |> String.concat ""
      in
      let const_c i = 
        [
          "r" ^ string_of_int (B.to_int i.rw) |> td;
          "C" |> td;
          string_of_int (B.to_int i.const) |> td ~style:"colspan=\"6\"";
        ] |> String.concat ""
      in
      let const_b i = 
        [
          B.to_string i.rw |> td;
          "1" |> td;
          B.to_string i.const |> td ~style:"colspan=\"6\"";
        ] |> String.concat ""
      in
      (match B.to_int i.lc, binary with
      | 0, false -> instr_c
      | 0, true -> instr_b
      | _, false -> const_c
      | _, _ -> const_b) i

    let rec td_of_asm_instr ?(binary=false) ?(map=(fun s -> s)) i = 
      let td ?style x = tag ?style "td" (map x) in
      match i with
      | Asm.Instr i | Asm.Const i -> [ td_of_instr ~binary ~map (decode i) ]
      | Asm.ConstLabel(rw,lab) ->
        [ if not binary then
          [
            "r" ^ string_of_int rw |> td; 
            "C" |> td;
            lab |> td ~style:"colspan=\"6\"";
          ] |> String.concat ""
        else
          [
            B.to_string (B.consti 7 rw) |> td;
            "1" |> td;
            lab |> td ~style:"colspan=\"6\"";
          ] |> String.concat "" ]
      | Asm.Block b ->
        List.flatten (List.map (td_of_asm_instr ~binary ~map) b)

    let html_of_asm_instr ?(binary=false) ?(map=(fun s -> tag "center" s)) i = 
      tag "table" ((List.map (tag "tr") (td_of_asm_instr ~binary ~map i)) |> String.concat "")

    let html_of_program ?(binary=false) ?(map=(fun s -> tag "center" s)) prog = 
      List.map (fun (label,asm) ->
        let td = td_of_asm_instr ~binary ~map (Asm.block asm) in
        let len = List.length td in
        let th = tag ~style:("rowspan=\"" ^ string_of_int (max len 1) ^ "\"") "td" label in
        let td = try (th ^ List.hd td) :: List.tl td with _ -> [th] in
        List.map (tag "tr") td
      ) prog |> List.concat |> String.concat "" |> tag "table"

    let html_of_assembled_program ?(binary=true) ?(map=(fun s -> tag "center" s)) 
      (Asm.Assembled i) = 
      let i = Array.map (fun i -> td_of_instr ~binary ~map (decode i)) i |> Array.to_list in
      tag "table" ((List.map (tag "tr") i) |> String.concat "")

    let html_of_interpreter (Asm.Assembled prog) interp = 
      let concat = String.concat "" in
      let pc = Printf.sprintf "%.8x" interp#pc in
      let inst = 
        let f b pc = 
          try tag "tr" 
            ((tag "td" (Printf.sprintf "%.8x" pc)) ^ 
            (td_of_instr 
              ~map:(if b then tag "b" else (fun s -> s)) 
              (decode prog.(pc))))
          with _ -> ""
        in
        tag "table" 
          ([ 
            f false (interp#pc-2);
            f false (interp#pc-1);
            f true interp#pc;
            f false (interp#pc+1);
            f false (interp#pc+2);
          ] |> concat)
      in
      let regs = 
        Array.init 16 (fun i ->
          Array.init 8 (fun j ->
            tag "td" (Printf.sprintf "%.8lx" (interp#reg (i*8+j)))))
      in
      let regs = Array.map (fun x -> tag "tr" (x |> Array.to_list |> concat)) regs in
      let regs = tag "table" (regs |> Array.to_list |> concat) in

      tag "table"
        ([
          tag "tr" ([ tag "td" "PC"; tag "td" (tag "center" pc) ] |> concat);
          tag "tr" ([ tag "td" "Instruction"; tag "td" (tag "center" inst) ] |> concat);
          tag "tr" ([ tag "td" "Registers"; tag "td" regs ] |> concat);
        ] |> concat)

  end

end


