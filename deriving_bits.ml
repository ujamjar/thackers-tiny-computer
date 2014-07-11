module type S = sig
  type t
  module Enum_t : Deriving_Enum.Enum with type a = t
  module Bounded_t : Deriving_Bounded.Bounded with type a = t
end

module type T = sig
  type b
  type t 
  val bits : int
  val to_bits : t -> b
  val of_bits : b -> t
  val case : b -> b -> (t * b) list -> b
  val (==:) : b -> t -> b
  module E : Deriving_Enum.Enum with type a = t
  module B : Deriving_Bounded.Bounded with type a = t
end

module Make(T : S)(B : HardCaml.Comb.S) = struct
  type b = B.t
  type t = T.t
  let bits = HardCaml.Utils.nbits (T.Enum_t.from_enum T.Bounded_t.max_bound)
  let to_bits a = B.consti bits (T.Enum_t.from_enum a)
  let of_bits a = T.Enum_t.to_enum (B.to_int a) 
  let case sel default l = 
    let a = Array.create (1 lsl bits) default in
    let () = List.iter (fun (i,x) -> a.(T.Enum_t.from_enum i) <- x) l in
    B.mux sel (Array.to_list a)
  let (==:) a b = B.(==:) a (to_bits b)
  module E = T.Enum_t
  module B = T.Bounded_t
end

