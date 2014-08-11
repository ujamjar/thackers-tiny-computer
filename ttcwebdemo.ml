open HardCaml
open Ttc
module B = Bits.Comb.IntbitsList
module A = Asm.Make(B)
module S = Ttc.Make(B)

(* ACE editor bindings *)

class type session = object
  method setMode : Js.js_string -> unit Js.meth
end

class type editor = object
  method setTheme : Js.js_string -> unit Js.meth
  method setValue : Js.js_string -> unit Js.meth
  method getValue : unit -> Js.js_string Js.t Js.meth
  method getSession : unit -> session Js.t Js.meth
end

class type ace = object
  method edit : Js.js_string -> editor Js.t Js.meth
end

(* utils *)

let debug = true
let jlog s = if debug then Firebug.console##log(s)
let log s = if debug then jlog (Js.string s)

let get_element e = 
    let d = Dom_html.document in
    Js.Opt.get 
        (d##getElementById (Js.string ("jsoo_" ^ e)))
        (fun () -> 
          log ("get_element failed: " ^ e);
          assert false)

let get_div e = 
  match Dom_html.tagged (get_element e) with
  | Dom_html.Div(div) -> div
  | _ -> failwith ("failed to get div " ^ e)

(* main logic and callbacks *)

let () = 
  Printf.printf "ttc webdemo\n";

  (* initialize the ace editor *)
  let ace : ace Js.t = Js.Unsafe.variable "ace" in
  let editor = ace##edit(Js.string "editor") in
  let () = editor##setTheme(Js.string "ace/theme/chrome") in
  let session = editor##getSession(()) in
  let () = session##setMode(Js.string "ace/mode/text") in

  let el_asm_result = get_element "asm_result" in
  let el_sim_data = get_element "simulator" in
  let el_compile = get_element "asm_compile" in
  let el_step = get_element "step" in

  (* compile code in asm editor *)
  let step = ref (fun () -> ()) in
  let compile () = 
    let program = editor##getValue(()) in
    let () = Printf.printf "%s" (Js.to_string program) in
    match A.Parser.parse_string (Js.to_string program) with
    | Some(program) -> begin
      let assembled = A.Asm.assemble program in
      let program_html = A.Printer.html_of_program program in
      let assembled_html = A.Printer.html_of_assembled_program assembled in
      el_asm_result##innerHTML <- Js.string 
("<table>
<tr>
  <td>opcodes</td>
  <td>disassembly</td>
</tr>
<tr>
  <td>" ^ program_html ^ "</td>
  <td>" ^ assembled_html ^ "</td>
</tr>
</table>");
      let sim = S.sim ~gtkwave:false ~input:None ~output:None ~im:assembled ~dm:[||] in
      let render () = 
        el_sim_data##innerHTML <- Js.string (A.Printer.html_of_interpreter assembled sim)
      in
      render();
      step := (fun () -> sim#step; render());
    end
    | None -> begin
      el_asm_result##innerHTML <- Js.string
        "<b>Failed to parser assembly<b>";
    end
  in

  el_compile##onclick <- Dom_html.handler (fun e -> compile (); Js._false);
  el_step##onclick <- Dom_html.handler (fun e -> (!step) (); Js._false);

