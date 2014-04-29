(** 

    Symbols extends the alphabet to include an empty and universal symbol. A
    module of [SYMBOL_TYPE] can be made by invoking [Symbols.Make] on a module
    of type [SYMBOL_SEED]. Modules of [SYMBOL_TYPE] are of [X.X_TYPE] (a Tools
    module) but add empty and universal symbols. Thus, the Symbols Module
    represents a small extension of [X.X_TYPE] (and modules of type
    [SYMBOL_SEED] require a couple extra functions).  The extensions are
    included because they may be needed when the symbol in the finite state
    acceptor is a feature bundle as opposed to an alphabetic unit.  Note that
    modules of [SYMBOL_TYPE] are modules of [X.X_TYPE]

    author: Jeff Heinz
    last updated : July 16,2008

*)



(** The input signature to [Symbols.Make]. *)
module type SYMBOL_SEED =
  sig
    include X.X_TYPE

    (** Returns whether or not two elements are compatible. *)
    val are_compatible : t -> t -> bool
      
    (** Returns an element given two elements. This has the same type
    signature as [X.pair] but the idea here is that if you are forced to
    combine the elements somehow, you will use [conform] and not [pair]. *)
    val conform : t -> t -> t
  end



(** The output signature of [Symbols.Make]. *)
module type SYMBOL_TYPE =
  sig

    (** The following list summarizes the function in [X.X_TYPE].
	- The type of symbol element is [E|U|S(x)] where [x=X.x].
	- [name] is the string ["Symbol("^X.name^")"].
	- [compare] is the same as [Pervasives.compare].
	- [pair s1 s2] unifies the symbols. If [s1=S(x1)] and [s2=S(x2)] then
	unification occurs only if [x1] and [x2] are compatible in which case
	unification yields [S(X.pair x1 x2)].
	- [of_string] returns a symbol from a string representation.
	The string ["<E>"] denotes the empty symbol.
	The string ["<U>"] denotes the universal symbol.
	Otherwise the string [x] denotes an element of type [X.t] and
	[S(X.of_string x)] is returned. If [x] is the empty, i.e. [""] a
	failure is raised.
	- [to_string] returns a string representation of the symbol.
	- [print] prints the symbol followed by an endline.
	- [print_] prints the symbol followed but no endline.

    *)
    include X.X_TYPE

    (** Two symbols are unifiable iff one of them is universal, they are both
    empty, or they are compatible [x] types. *)
    val are_unifiable : t -> t -> bool

    (** If [s1] and [s2] are symbols of type [x] then [conform s1 s2] returns
    the value of [conform] for the elements as defined by the module of type
    [SYMBOL_SEED].  Otherwise it returns [s2].  *)
    val conform : t -> t -> t

    (** The type of X. *)
    type x

    (** Returns a symbol given [x]. *)
    val of_x : x -> t

    (** Returns an element of type [x] given a symbol. Raises a failure if the
	symbol is the the empty symbol [<E>] or the universal symbol [<U>]. *)
    val to_x : t -> x

    (** The empty symbol.  *)
    val blank : t

    (** The universal symbol.  *)
    val wild : t

  end

(** The functor takes as an argument modules of type [SYMBOL_SEED] and returns
    a module of [SYMBOL_TYPE]. *)
module Make (X : SYMBOL_SEED) : SYMBOL_TYPE with type x = X.t
