(** Vocab implements the alphabet used in finite state acceptor. Here this is
    done with strings.  Note that Vocab is of types
    {!Symbols.SYMBOL_SEED} and [X.X_TYPE].

    author: Jeff Heinz
    last updated : July 8, 2006 
*)


include Symbols.SYMBOL_SEED
    (** The following list summarizes the functions in {!Symbols.SYMBOL_SEED}.
	- Type [t] is a string, o.e. [Vocab] is implemented over strings. 
	- The name is ["Vocab"]. 
	- [compare] is the same as [Pervasives.compare]. 
	- [pair a1 a2] returns [a2]. 
	- [of_string] returns a vocublary element from its string representation. 
	- [to_string] returns a string representation of a vocabulary element. 
	- [print] prints a vocabulary element followed by an endline. 
	- [print_] prints a vocabulary element but no endline.
	- [are_compatible] is true iff two vocabulary elements are the same. 
	- [conform x1 x2] returns [x2], i.e. [conform = pair]. *)
  
