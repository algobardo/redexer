module type SEMIRING_TYPE =
sig
  include X.X_TYPE
  val times : t -> t -> t
  val plus : t -> t -> t
  val zero : t
  val one : t
end

module type FLOAT_TYPE =
sig
  include SEMIRING_TYPE
  val to_float : t -> float
  val of_float : float -> t
end


module type INT_TYPE =
sig
  include SEMIRING_TYPE
  val to_int : t -> int
  val of_int : int -> t
end


module type BOOL_SEED = 
sig
  val name: string
  val pair: bool -> bool -> bool
end

module Make_Bool (Core:BOOL_SEED) : SEMIRING_TYPE

module type STRING_SEED = 
sig
  val name: string
  val pair: string -> string -> string
end

module Make_String (Core:STRING_SEED) : SEMIRING_TYPE

module type INT_SEED = 
sig
  val name: string
  val pair: int -> int -> int
end

module Make_Ordinal_Tropical (Core:INT_SEED) : SEMIRING_TYPE
module Make_Frequency (Core:INT_SEED): INT_TYPE

module type FLOAT_SEED = 
sig
  val name: string
  val pair: float -> float -> float
end

module Make_Probability (Core:FLOAT_SEED) : FLOAT_TYPE
