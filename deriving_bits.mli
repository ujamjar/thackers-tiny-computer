(*
Usage:

module X = Deriving_bits.Make(struct
  type t = Add | Sub | Inc
  deriving(Enum,Bounded)
end)(Bits)

*)
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

module Make(T : S)(B : HardCaml.Comb.S) : T
  with type b = B.t
   and type t = T.t

