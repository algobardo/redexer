(** Edges implements the transitions of finite state acceptors.  Transitions
    have three components: an origin node, a terminus node, and a label. The
    module [Make] takes three modules as arguments and returns a module of
    [EDGE_TYPE].  The first module is of [X.X_TYPE] and will represent the
    vertices of the edge. The second module is also of [X.X_TYPE] and
    provides the label on the edge. The third module is of [Delim.DELIM_TYPE]
    and provides a way for a string representation of the edge.

    author: Jeff Heinz
    last updated : July 6, 2006

*)



(** The output signature of [Make]. *)
module type EDGE_TYPE = 
sig
  include X.X_TYPE
    (** The following list summarizes the function in [X.X_TYPE].
	- Type [t] is a record, i.e. edges have type [{origin:node;terminus:node;label:label}]. 
	- [name] is ["Edge"]. 
	- [compare] is [compare]. 
	- [pair e1 e2] returns an edge formed by pairing its components.
	- [of_string] returns an edge from its string representation.  The
	left brace/tag and right brace/tag of the string are given by the the
	functions [D.lb] and [D.rb]. These can be empty strings. The
	components of the edge will be delimited by [D.delim] which {b cannot}
	be empty.
	- [to_string] returns a string representation of an edge. 
	- [print] prints an edge followed by an endline.
	- [print_] prints an edge but no endline.

 *)

  (** The vertex of an edge has this type.*)
  type node

  (** The label of an edge has this type.*)
  type label

  (** Returns the origin of an edge. *)
  val origin : t -> node

  (** Returns the terminus of an edge. *)
  val terminus : t -> node

  (** Returns the label of an edge. *)
  val label : t -> label

  (** [make o t l] Returns an edge where the origin is [o], the terminus is
      [t] and the label is [l].  *)
  val make : node -> node -> label -> t

  (** This is the compare function for labels. I.e. [Edge.label_compare =
      Label.compare]. *)
  val label_compare : label -> label -> int

  (** This function returns a string representation of the edge that can be interpreted by AT&T's {{:http://graphviz.org/Documentation.php}Graphviz} drawing
      program. *)
  val to_dotstring : t -> string
end


(** The functor takes the three modules below and returns a module of
    EDGE_TYPE. *)
module Make 
  (Node: X.X_TYPE)
  (Label : X.X_TYPE)
  (D: Delim.DELIM_TYPE) :
  
  EDGE_TYPE with type label = Label.t
	    and type node = Node.t

