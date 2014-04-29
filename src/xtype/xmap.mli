(**
  Xmap implements some extra functions over maps.

  author: Jeff Heinz

  last updated: August 4, 2006

*)


(** The output signature of [Make]. *)
module type XMAP_TYPE =
  sig

(** {2 X_TYPE functions}*)

    include X.X_TYPE
    (** The following list summarizes the functions in {!X.X_TYPE}.
	- Type [t] is the map.
	- [name] is ["Map ("^(Key.name)^" -> "^Data.name^")"]. 
	- [compare m1 m2] is the same as [Map.Make.compare Data.compare m1 m2]. 
	- [pair] is an intersection of maps.
	- [of_string] returns a map from its string representation. 
	- [to_string] returns a string representation of a map. 
	- [print] prints a map followed by an endline.
	- [print_] prints a map but no endline.
    *)

(** {2 XMAP_TYPE functions}*)

    (** The following functions are the corresponding functions in [Map.Make] in
    the standard library. *)

    (** The type of key in the map. (I.e. the domain of the function.) *)
    type key

    (** The type of data in the map. (I.e. the range of the function.) *)
    type data

    (** [relation_delim] is the string used to indicate the mapping
	from keys to data in string representations of the map. *)
    val relation_delim : string

    (** [elt_delim] is the string used to delimit the elements
	(i.e. (key -> data) mappings) in the map in string
	representations of the map. *)
    val elt_delim : string

    val empty: t
    val choose: t -> key * data
    val is_empty: t -> bool
    val add: key -> data -> t -> t
    val find: key -> t -> data
    val remove: key -> t -> t
    val mem:  key -> t -> bool
    val iter: (key -> data -> unit) -> t -> unit
    val fold: (key -> data -> 'a -> 'a) -> t -> 'a -> 'a 
    val equal: t -> t -> bool

    (** {2 Extra functions} *)

    (** [filter p m] returns all the keys and data in [m] which satisfy predicate [p] *)
    val filter: (key -> data -> bool) -> t -> t

    (** [cardinal m] returns the number of key bindings in [m] *)
    val cardinal: t -> int

    (** Writes the map to a file. *)
    val to_file: string -> t -> unit

    (** Returns a map that has been written in a file. *)
    val of_file: string -> t

    (** Returns the number of mappings in the map. *)
    val size: t -> int
  end



(** The functor [Make] takes three modules and outputs a module of XMAP_TYPE. *)
module Make 
  (D: Delim.DELIM_TYPE) 
  (Key: X.X_TYPE)          (* domain *)
  (Data: X.X_TYPE) :       (* range  *)

  XMAP_TYPE with type key = Key.t 
	    and type data = Data.t 

