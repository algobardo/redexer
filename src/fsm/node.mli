(** Node implements the states of a finite state acceptors. Here the node type
    is [string]. The Node module is of type [X.X_TYPE] (a Tools module).

    author: Jeff Heinz
    last updated : July 6, 2006
*)

include X.X_TYPE
    (** The following list summarizes the function in [X.X_TYPE].
	- Type [t] is a string, i.e. nodes are implemented over strings.
	- [name] is ["Node"]. 
	- [compare] is the same as [Pervasives.compare]. 
	- [pair n1 n2] returns a node formed from the string [n1^"_"^n2]. Pairing of
	nodes is used when state merging, for example. 
	- [of_string] returns a node from its string representation. 
	- [to_string] returns a string representation of a node. 
	- [print] prints a node followed by an endline.
	- [print_] prints a node but no endline.

 *)

