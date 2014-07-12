open Printf

module B = HardCaml.Bits.Comb.IntbitsList
module A = Asm.Make(B)

let () = 
  A.Parser.with_file Sys.argv.(1) (fun chan ->
    match A.Parser.parse_channel chan with
    | None -> eprintf "Failed to parse assembly.\n"
    | Some(p) -> begin
      let A.Asm.Assembled p = A.Asm.assemble p in
      Array.iter (fun x -> Printf.printf "%s\n" (B.to_string x)) p
    end
  )

