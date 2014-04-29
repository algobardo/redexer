(**
   Machines implements basic functions of any type of finite state
   machine. Thus when building particular types of finite state machines,
   instead of recoding the same functions, they can just be called on here.

   author: Jeff Heinz
   last updated: July 15, 2006
*)

(** The output signature of [Machines.Make]. *)
module type MACHINE_TYPE =
sig
  include X.X_TYPE
    (** The following list summarizes the function in [X.X_TYPE].
	- Type [t] is a record, i.e. machines have type [{starts:nodeSet;finals:nodeSet;edges:edgeSet}]. 
	- [name] is ["FSM"]. 
	- [compare] is [compare]. 
	- [pair] is the intersection. See the function [inter] below.
	- [of_string] returns a FSM from its string representation.  The
	left brace/tag and right brace/tag of the string are given by the the
	functions [D.lb] and [D.rb]. These can be empty strings. The
	components of the machine will be delimited by [D.delim] which {b cannot}
	be empty.
	- [to_string] returns a string representation of a machine.
	- [print] prints a machine followed by an endline.
	- [print_] prints a machine but no endline.

    *)

  (** Sets of nodes in a machine have this type.  *)
  type nodeSet

  (** Sets of edges in a machine have this type. *)
  type edgeSet

  (** The empty machine. *)
  val empty : t

  (** [make s f e] returns a machine with start states equal to [s], final
      states equal to [f] and edges equal to [e]. *)
  val make : nodeSet -> nodeSet -> edgeSet -> t
    
    (** [of_file filename] returns a finite state machine from reading file
	[filename]. *)
  val of_file : string -> t

  (** [to_file filename fsm] writes [fsm] to file [graphname].*)
  val to_file : string -> t -> unit

  (** [to_dotfile ?size ?rd ?shape ?fs path filename fsm] returns a
      string representation of [fsm] that can be interpreted by AT&T's
      {{:http://graphviz.org/}Graphviz} drawing program.  Optionally
      you can specify the [size], the rank direction [rd], the shape of the
      nodes [shape], and the fontsize to be used in edges and nodes [fs]. See
      the {{:http://graphviz.org/Documentation.php}Graphviz documentation} for
      more information about these options.
      *)

  val to_dotstring : ?size:string -> ?rd:string -> ?shape:string -> 
    ?fs:string -> string -> t -> string

  (** [to_dotfile ?size ?rd ?shape ?fs path graphname fsm] writes a string
      representation of [fsm] that can be interpreted by AT&T's
      {{:http://graphviz.org/}Graphviz} drawing program to a file. The name of
      the file is [graphname] and it is written in location [path] (This
      division is necessary because not all legal graph names in Graphviz are
      filenames, e.g. graph names cannot begin with numerals). As in
      [to_dotstring] you may optionally specify the [size], the rank direction
      [rd], the shape of the nodes [shape], and the fontsize to be used in
      edges and nodes [fs]. *)
  val to_dotfile : ?size:string -> ?rd:string -> ?shape:string -> 
    ?fs:string -> string -> string -> t -> unit

  (** Prints to standard output a summary of the size of the machine *)
  val report : t -> unit

  (** {2 Properties of machines} *)

  (** Returns the start states of the machine. *)
  val starts : t -> nodeSet
  
  (** Returns the final states of the machine. *)
  val finals : t -> nodeSet

  (** Returns the edges of the machine. *)
  val edges : t -> edgeSet

  (** Returns the nodes of the machine. *)
  val nodes : t -> nodeSet

  (** Returns [true] iff the machine is cyclic. *)
  val is_cyclic : t -> bool

  (** Returns [true] iff the machine has no useless states. *)
  val is_stripped : t -> bool

  (** {2 Comparing machines} *)

  (** Returns [true] iff the two machines are isomorphic. *)
  val are_isomorphic : t -> t -> bool
    
  (** Returns [true] iff the two machines have equal components. *)
  val equal : t -> t -> bool

  (** {2 Unary operations} *)


  (** Returns the reverse machine *)
  val reverse : t -> t

  (** Returns the prefixal machine (i.e. all states are now final) *)
  val prefix : t -> t

  (** Returns the suffixal machine (i.e. all states are now start) *)
  val suffix : t -> t

  (** Returns a machine with no useless states. *)
  val trim : t -> t

  (** Renames the nodes of a machine. The new names are based on the positive
      integers.*)
  val rename : t -> t

  (** [rename_n n m] renames the nodes of a machine beginning with integer [n].*)
  val rename_n : int -> t -> t

  (** {2 Binary operations} *)

  (** [inter m1 m2] returns the intersection of [t1] and [t2]. *)
  val inter : t -> t -> t

  (** [union m1 m2] returns a machine which is the union of the two
  machines. Node names are renamed if there is a conflict. *)
  val union : t -> t -> t
end


(** The functor takes the arguments below and returns a module of type [MACHINE_TYPE].
*)  
module Make
  (Node:X.X_TYPE) 
  (NodeSet:Xset.XSET_TYPE with type elt = Node.t) 
  (Edge:Edges.EDGE_TYPE with type node = Node.t) 
  (EdgeSet:Xset.XSET_TYPE with type elt = Edge.t) 
  (D:Delim.DELIM_TYPE) :
  
  MACHINE_TYPE with type nodeSet = NodeSet.t
	       and type edgeSet = EdgeSet.t
  


