
(** 
    This module type establishes basic functions an abstract type must
    have. Modules of this tye can be used in in functors to generate module
    types of XSET_TYPE and XLIST_TYPE, which are themselves of type X_TYPE.
    
    author: Jeff Heinz
    
    last updated: May 1, 2009
    
*)

module type X_TYPE =
sig
  (** The type this module is built around. *) 
  type t
    
  (** The name; i.e. "Point" or "Line". *)
  val name: string
    
  (** The compare function-- modules of X_TYPE have ordered types. *)
  val compare: t -> t -> int
    
  (** A binary operator for X_TYPE.  If pairing two elements of type [t] is
      undefined the function should [raise (Failure("NonCombinable"))]. *)
  val pair: t -> t -> t
    
  (**  Returns an element of type [t] from a string representation. *)
  val of_string: string -> t
    
  (** Returns a string representation of element [t]. *)
  val to_string: t -> string 
    
    
  (** The two printing functions have the same type signatures but they can
      be implemented differently. Both should print the string
      representation of [t] to an [out_channel oc]. This out channel is
      optional and by default it is set to [stdout] (standard
      output).  Currently I usually define [print] as writing an endline
      character and [print_] as not. *)
    
    
  val print: ?oc:out_channel -> t -> unit
  val print_ : ?oc:out_channel -> t -> unit 
end
  
  
module type PAIR_TYPE =
sig
  include X_TYPE
  type first
  type second
  val first : t -> first
  val second : t -> second
  val make : first -> second -> t
end

module Make_Pair 
  (D: Delim.DELIM_TYPE) 
  (First: X_TYPE)
  (Second : X_TYPE) :

  
  PAIR_TYPE with type first = First.t
	    and type second = Second.t



module type TRIPLE_TYPE =
sig
  include X_TYPE
  type first
  type second
  type third
  val first : t -> first
  val second : t -> second
  val third : t -> third
  val make : first -> second -> third -> t
end


module Make_Triple
  (D: Delim.DELIM_TYPE) 
  (First: X_TYPE)
  (Second : X_TYPE)
  (Third : X_TYPE) :
  
  TRIPLE_TYPE with type first = First.t
	    and type second = Second.t
	    and type third = Third.t

