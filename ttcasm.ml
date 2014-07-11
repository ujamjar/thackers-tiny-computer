open Printf

module B = HardCaml.Bits.Comb.IntbitsList
module A = Asm.Make(B)

let () = 
  A.Parser.with_file Sys.argv.(1) (fun chan ->
    match A.Parser.parse_channel chan with
    | None -> eprintf "Failed to parse assembly.\n"
    | Some(x) -> begin
      eprintf "Parsed assembly.\n";
      A.Printer.pretty print_string x
    end
  )

