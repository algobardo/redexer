module type SEMIRING_TYPE = 
sig
  include X.X_TYPE
  val times : t -> t -> t
  val plus : t -> t -> t
  val zero : t
  val one : t
end

module type FLOAT_TYPE=
sig
  include SEMIRING_TYPE
  val to_float : t -> float
  val of_float : float -> t
end


module type INT_TYPE=
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

module type STRING_SEED =
sig
  val name: string
  val pair: string -> string -> string
end

module type INT_SEED =
sig
  val name: string
  val pair: int -> int -> int
end

module type FLOAT_SEED =
sig
  val name: string
  val pair: float -> float -> float
end


module Make_Bool (Core:BOOL_SEED) =
struct
  let name = Core.name
  type t = bool
  let compare = compare
  let pair = Core.pair
  let of_string s = 
    try bool_of_string s
    with _ -> raise (Failure(".of_string: Unrecognizable Value"))
	  
  let to_string = function
      true -> "true"
    | false -> "false"

  let one = true
  let zero = false
  let times a b = a && b
  let plus a b = a || b 

  let print = Print.endline to_string
  let print_ = Print.plain to_string
end


module Make_String (Core:STRING_SEED) =
struct
  let name = Core.name
  type t = S of string | Inf
  let compare = compare
  let pair x y = match (x,y) with
      (Inf,_) -> Inf
    | (_,Inf) -> Inf
    | (S(x),S(y)) -> S(Core.pair x y)

  let of_string x = match x with 
      "<Inf>" -> Inf
    | _ -> S(x)

  let to_string x = match x with 
      Inf -> "<Inf>"
    | S(y) -> y
    
  let one = S("")
  let zero = Inf

  let plus a b = 
    (* this needs to be changed to longest common prefix *)
    match (a,b) with
	(Inf,Inf) -> Inf
      | (S(x),S(y)) -> S(x^y)
      | (S(x),_) -> Inf
      | (_,S(x)) -> Inf
	  
  let times a b = 
    match (a,b) with
      (Inf,Inf) -> Inf
    | (S(x),S(y)) -> S(x^y)
    | (S(x),_) -> S(x)
    | (_,S(x)) -> S(x)

  let print = Print.endline to_string
  let print_ = Print.plain to_string
end



module Make_Frequency (Core:INT_SEED) =
struct
  let name = Core.name
  type t = int
  let compare = compare
  let pair = Core.pair

  let of_string = int_of_string
  let to_string = string_of_int
    
  let one = 1
  let zero = 0
  let times x y = x*y 
  let plus x y = x+y

  let to_int t = t
  let of_int t = t

  let print = Print.endline to_string
  let print_ = Print.plain to_string
end

module Make_Ordinal_Tropical (Core:INT_SEED) =
struct
  let name = Core.name
  type t =  NegInf | Ordinal of int | PosInf
  let compare x y = 
    match x,y with
	(NegInf,_) -> -1 
      | (_,PosInf) -> -1
      | (_,NegInf) -> 1
      | (PosInf,_) -> 1
      | (Ordinal(i1),Ordinal(i2)) -> compare i1 i2

  let of_string = function
     "<PosInf>" -> PosInf
    |"<NegInf>" -> NegInf
    | s -> Ordinal(int_of_string s)

  let to_string = function
     PosInf -> "<PosInf>"
    | NegInf -> "<NegInf>"
    | Ordinal(i) -> string_of_int i
    
  let one = Ordinal(0)
  let zero = NegInf

  let times x y = 
    (* times is + *)
    match x,y with
	_,PosInf -> PosInf
      | PosInf,_ -> PosInf
      | _,NegInf -> NegInf
      |	NegInf,_ -> NegInf
      | Ordinal(i1),Ordinal(i2) -> Ordinal(i1 + i2)

  let plus x y = 
    (* plus is min *)
    match x,y with
	_,NegInf -> NegInf
      |	NegInf,_ -> NegInf
      |	_,PosInf -> x
      | PosInf,_ -> y
      | Ordinal(i1),Ordinal(i2) -> if i1 < i2 then Ordinal(i1) else Ordinal(i2)

  let pair = times

  let print = Print.endline to_string
  let print_ = Print.plain to_string
end


module Make_Probability (Core:FLOAT_SEED) =
struct
  let name = Core.name
  type t = float
  let compare = compare
  let pair = Core.pair
  let of_string = float_of_string
  let to_string = string_of_float
    
  let one = 1.
  let zero = 0.
  let times a b = a*.b
  let plus a b = a+.b
    
  let to_float t = t
  let of_float t = t

  let print = Print.endline to_string
  let print_ = Print.plain to_string
end
