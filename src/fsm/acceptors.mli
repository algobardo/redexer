(** Acceptors implements finite state acceptors. 

    author: Jeff Heinz
    last updated : July 25,206

*)


(** The output signature of [Acceptors.Make], [Acceptors.Make_Q] and
    [Acceptors.Make_DQ]. *)
module type ACCEPTOR_TYPE =
sig
  include Machines.MACHINE_TYPE
    (** See {!Machines.MACHINE_TYPE} for basic functions. 
    *)

  (** {2 Types} *)

  (** States in the finite state acceptor have this type. *)
  type node

  (** Transitions in the finite state acceptor have this type. *)
  type edge

  (** Labels of transitions in the finite state acceptor have this type. *)
  type label

  (** Words that can or cannot be accepted by the finite state acceptor have
      this type. *)
  type word 

  (** Sets of words have this type. *)
  type wordSet

  (** Partitions of node sets have this type *)
  type nodeSetSet

(*  type edgeMap *)

  (** {2 FSA basics} *)

  (** Returns [true] iff the finite state acceptor is deterministic. *)
  val is_deterministic : t -> bool

  (** Returns a determinized version of the finite state acceptor such that
      there is an edge for every (state,label) pair. *)
  val complete_determinize : t -> t   

  (** Returns a determinized version of the finite state acceptor which has
      trimmed useless states and edges. *)
  val determinize : t -> t   

  (** Returns the smallest deterministic acceptor given some acceptor. *)  
  val minimize : t -> t

  (** Returns the complement acceptor. *)
  val complement : t -> t

(*
(** {2 Tools} *)
  val edgeSet_to_edgeMap : edgeSet -> edgeMap
  val edgeMap_to_edgeSet : edgeMap -> edgeSet
  val edgeMap_find : (node * label) -> edgeMap -> nodeSet
*)
  (** Prints to standard output the edges of the acceptor as a map from
      (state,label) pairs to node sets.*)
  val print_edgeMap : t -> unit

  (** [concat fsa1 fsa2] returns an acceptor which accepts words in the language formed by
      concatenating the language of [fsa1] with the language of [fsa2]. *)
  val concat : t -> t -> t

  (** Returns an acceptor which accepts words in the language formed by taking
      the star closure of the language of the input fsa. *)
  val star : t -> t


  (** {2  Properties} *)

  (** [is_nd j k t] returns true iff [t] is j-k-neighborhood-distinct. *)
  val is_nd : int -> int -> t -> bool

  (** {2 Accepting words} *)

  (** [transform beginning_nodes word] returns the nodeset that can be reached
      from [beginning_nodes] with [word].  *)
  val transform : t -> nodeSet -> word -> nodeSet

  (** Returns [true] iff the acceptor accepts the word represented by the
      string. *) 
  val accepts : t -> string -> bool

  (** Returns a pair of wordsets: the first member of the pair are the words
    accepted by the acceptor, the second member are those words not accepted. *)
  val accepts_wordSet : t -> wordSet -> wordSet * wordSet

  (** [generate fsa min max] Returns a set of words of length between [min] and [max] that [fsa] accepts.*) 
  val generate : t -> int -> int -> wordSet

  (** [generate_p fsa min max] Prints to standard output words of length [min] and [max] that [fsa] accepts.*) 
  val generate_p : t -> int -> int -> unit

  (** [k_followers n nodeSet edgeSet] returns all words of length less than or
      equal to [n] that can be reached from [nodeSet].*)
  val k_followers : int -> nodeSet -> edgeSet -> wordSet

    (** [k_leaders n nodeSet edgeSet] returns all words of length less than or
	equal to [n] for which there exists a path to a node in [nodeSet].*)
  val k_leaders : int -> nodeSet -> edgeSet -> wordSet

  (** Prints a word set to standard output as default, otherwise to [~oc]. *)
  val print_wordSet :  ?oc:out_channel -> wordSet -> unit
    
  (** {2 Prefix trees and fsa extension} *)

  (** [extend_pt fsa word] extends [fsa]. This function depends on the user to
      ensure that the [fsa] is an actual prefix tree (i.e. the names of the
      nodes are string representations of prefixes of the words). For
      extending any acceptor use [extend] below. *)
  val extend_pt : t -> word -> t

  (** Returns a prefix tree from a set of words. *)
  val pt : wordSet -> t

  (** Returns a suffix tree from a set of words. *)
  val st : wordSet -> t

  (** [make_pt filename] returns a prefix tree from a file containing a list of
      words. The nodes are named with the prefixes. *)
  val make_pt : string -> t

  (** [make_st filename] returns a suffix tree from a file containing a list
      of words. The nodes are named with prefixes of the reverse language;
      that is, reverse the node name to obtain the suffix. *)
  val make_st : string -> t

  (** [extend fsa word] returns an acceptor which adds [word] to the language
      it accepts. *)
  val extend : t -> word -> t

  (** [make_pt2 filename] returns a prefix tree from a file containing a list
      of words. Runs faster than [make_pt], but each node is named with a
      number, not a prefix. *)
  val make_pt2 : string -> t

  (** [make_st2 filename] returns a suffix tree from a file containing a list
      of words. Runs faster than [make_st], but each node is named with a
      number. *)
  val make_st2 : string -> t

  (** Returns a word set from a string of words. *)
  val wordSet_of_string : string -> wordSet

  val make_fin : in_channel -> t


  (** [shuffle_ideal w1 w2] returns a fsa which accepts the shuffle
      ideal of [w2]. [w1] is a word that contains all symbols in the
      alphabet.*)
  val shuffle_ideal : word -> word -> t

  (** [factor_ideal w1 w2] returns a fsa which accepts the factor
      ideal of [w2]. [w1] is a word that contains all symbols in the
      alphabet.*)
  val factor_ideal : word -> word -> t

  (** [sigma_star w] returns a fsa which accepts sigma star, where [w]
      is a word that contains all symbols in the alphabet.*)
  val sigma_star : word -> t


  (** {2 Range FSAs} *)

  (** Returns a range distinct acceptor from a set of words. *)
  val range_fsa : wordSet -> t

  (** [make_range-fsa filename] returns an acceptor for the samllest
      range-distinct language which accepts the words in [filename]. *)
  val make_range_fsa : string -> t


  (** {2 State merging} *)

  (** [merge1 fsa pi] returns the acceptor which results from merging the
      blocks of partition [pi]. *)
  val merge1 : t -> nodeSetSet -> t

  (** [merge1_ns fsa ns] returns the acceptor which results from merging the
      nodes in [ns]. *)
  val merge1_ns : t -> nodeSet -> t

  (** [merge fsa equiv_relation] ultimately returns an acceptor in which the
      partition of the states of [fsa] induced by [equiv_relation] is the
      trivial partition (i.e. each state is in its own block).  It repeatedly
      merges the partition induced by [equiv_relation] until this state of
      affairs occurs. *)
  val merge : t -> (t -> nodeSetSet) -> t

  (** {2 Various partitions of FSAs} *)

  (** Returns a partition of the states of the acceptor according to whether
      or not they share b-successors. (See Angluin 1982).*)
  val b_successors : t -> nodeSetSet

  (** Returns a partition of the states of the acceptor so that all final
      states are in the same block. *)
  val is_final_eqr : t -> nodeSetSet

  (** Returns a partition of the states of the acceptor so that all nonfinal
      states are in the same block. *)
  val is_nonfinal_eqr : t -> nodeSetSet

  (** Returns a partition of the states of the acceptor so that all start
      states are in the same block. *)
  val is_start_eqr : t -> nodeSetSet

  (** Returns a partition of the states of the acceptor so that all nonstart
      states are in the same block. *)
  val is_nonstart_eqr : t -> nodeSetSet

  (** Returns a partition of the states of the acceptor so that all states
      with the same k_leaders are in the same block. *)
  val k_leaders_eqr : int -> t -> nodeSetSet

  (** Returns a partition of the states of the acceptor so that all states
      with the same k_followers are in the same block. *)
  val k_followers_eqr : int -> t -> nodeSetSet

  (** [jk_nhoods j k fsa] returns a partition of the states of [fsa] according
      so that states with the same j-k-neighborhood are in the same block. *)
  val jk_nhoods_eqr : int -> int -> t -> nodeSetSet

(*
  (** [ngrams n fsa] Returns a parition of the states of [fsa] according to
      whether they have the same (n-1)-0-neighborhood. *)
  val ngrams : int -> t -> nodeSetSet
*)
    (* 
       val lda : t -> nodeSetSet
    *)

end


module Make
  (Vocab:Symbols.SYMBOL_SEED) 
  (Word:Xlist.XLIST_TYPE with type elt = Vocab.t) 
  (WordSet:Xset.XSET_TYPE with type elt = Word.t) 
  (Symbol:Symbols.SYMBOL_TYPE with type x = Vocab.t) 
  (Node:X.X_TYPE) 
  (NodeSet:Xset.XSET_TYPE with type elt = Node.t) 
  (NodeSetSet:Xset.XSET_TYPE with type elt = NodeSet.t)
  (Edge:Edges.EDGE_TYPE with type node = Node.t 
			and type label = Symbol.t) 
  (EdgeSet:Xset.XSET_TYPE with type elt = Edge.t) 
  (D:Delim.DELIM_TYPE) :

   ACCEPTOR_TYPE with type node = Node.t
		 and type nodeSet = NodeSet.t
		 and type edge = Edge.t
		 and type edgeSet = EdgeSet.t
		 and type label = Symbol.t
		 and type word = Word.t
		 and type wordSet = WordSet.t



 (** This functor also returns a module of [ACCEPTOR_TYPE], but it only takes
     two arguments.  The remainder are generated internally using default
     delimiter modules. *)
module Make_Q
  (Vocab:Symbols.SYMBOL_SEED)
  (Node:X.X_TYPE) :
  ACCEPTOR_TYPE with type node = Node.t


(** This functor takes only arguments of type [Delim.DELIM_TYPE] (a Tools module).  It is
not as quick [Make_Q] but allows you to specify how FSAs will be written. *)
module Make_DQ 
  (Vocab:Symbols.SYMBOL_SEED) 
  (Node:X.X_TYPE) 
  (WordD:Delim.DELIM_TYPE) 
  (WordSetD:Delim.DELIM_TYPE) 
  (EdgeD:Delim.DELIM_TYPE) 
  (NodeSetD:Delim.DELIM_TYPE) 
  (NodeSetSetD:Delim.DELIM_TYPE) 
  (EdgeSetD:Delim.DELIM_TYPE) 
  (D:Delim.DELIM_TYPE):
  ACCEPTOR_TYPE with type node = Node.t

