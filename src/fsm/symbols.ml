module type SYMBOL_SEED = 
sig
  include X.X_TYPE
  val are_compatible : t -> t -> bool
  val conform : t -> t -> t
end

module type SYMBOL_TYPE = 
sig
  include X.X_TYPE
  val are_unifiable : t -> t -> bool
  val conform : t -> t -> t
  type x
  val of_x : x -> t
  val to_x : t -> x
  val blank : t
  val wild : t
end

	
module Make 
  (X : SYMBOL_SEED) =

struct

  type x = X.t
  type t = U | E | S of x
  let blank = E  (* Empty symbol *)
  let wild = U  (* Universal matching symbol *)

  let name = ("Symbol ("^(X.name)^")")

  let compare s1 s2 = 
    match (s1,s2) with      (* S(x) < E < U *)
      | (E,E) | (U,U) -> 0 
      | (_,U) -> -1 
      | (U,_) -> 1 
      | (_,E) -> -1 
      | (E,_) -> 1
      | (S(x),S(y)) -> X.compare x y

   let pair s1 s2 =    (* this used to be called unify *)
     match (s1,s2) with 
       | (S(x),S(y)) when X.are_compatible x y -> S(X.pair x y)
       | (E,E) -> E 
       | (U,_) -> s2 
       | (_,U) -> s1
       | _ -> raise (Failure("NonCombinable"))

   let are_unifiable s1 s2 =
     try let _ = pair s1 s2 in true
     with (Failure("NonCombinable")) -> false

   let conform s1 s2 =
     match (s1,s2) with 
       |(S(x),S(y)) -> S(X.conform x y)
       | _ -> s2

   let of_x x = S(x)
   let to_x s = 
     match s with 
       | S(x) -> x
       | E -> raise (Failure("Symbol.to_x: <E> is not of type x"))
       | U -> raise (Failure("Symbol.to_x: <U> is not of type x"))

   let to_string symbol =
     match symbol with
       | U -> "<U>"
       | E -> "<E>"
       | S(x) -> X.to_string x

   let of_string s = 
    match s with 
      | "<U>" -> U
      | "<E>" -> E
      | "" -> failwith "Symbol.of_string: detected an empty string."
      | _ -> S(X.of_string s)

   let print ?oc:(oc=stdout) symbol = 
     output_string oc (to_string symbol);
     output_string oc "\n";
     flush oc

   let print_ ?oc:(oc=stdout) symbol = 
     output_string oc (to_string symbol);
     flush oc


end
