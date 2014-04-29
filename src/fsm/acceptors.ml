module type ACCEPTOR_TYPE =
sig
  include Machines.MACHINE_TYPE
  type node
  type edge
  type label
  type word 
  type wordSet
  type nodeSetSet

(*  type edgeMap *)

  (* FSA basics *)
  val is_deterministic : t -> bool
  val complete_determinize : t -> t   
  val determinize : t -> t   
  val minimize : t -> t
  val complement : t -> t
  val print_edgeMap : t -> unit

  (* operations *)
  val concat : t -> t -> t
  val star : t -> t

  (* properties *)
  val is_nd : int -> int -> t -> bool

  (* accepting words *)
  val transform : t -> nodeSet -> word -> nodeSet
  val accepts : t -> string -> bool
  val accepts_wordSet : t -> wordSet -> wordSet * wordSet

  val generate : t -> int -> int -> wordSet
  val generate_p : t -> int -> int -> unit
  val k_followers : int -> nodeSet -> edgeSet -> wordSet
  val k_leaders : int -> nodeSet -> edgeSet -> wordSet
  val print_wordSet : ?oc:out_channel -> wordSet -> unit
  
  (* prefix trees and fsa extension *)
  val extend_pt : t -> word -> t
  val pt : wordSet -> t
  val st : wordSet -> t
  val make_pt : string -> t
  val make_st : string -> t
  val extend : t -> word -> t
  val make_pt2 : string -> t
  val make_st2 : string -> t
  val wordSet_of_string : string -> wordSet
  val make_fin : in_channel -> t

  (* making acceptors *)
  val shuffle_ideal : word -> word -> t
  val factor_ideal :  word -> word -> t
  val sigma_star : word -> t

  (* range fsas *)
  val range_fsa : wordSet -> t
  val make_range_fsa : string -> t

  (* state merging *)
  val merge1 : t -> nodeSetSet -> t
  val merge1_ns : t -> nodeSet -> t
  val merge : t -> (t -> nodeSetSet) -> t

  (* various partitions of FSAs *)
  val b_successors : t -> nodeSetSet
  val is_final_eqr : t -> nodeSetSet
  val is_nonfinal_eqr : t -> nodeSetSet
  val is_start_eqr : t -> nodeSetSet
  val is_nonstart_eqr : t -> nodeSetSet
  val k_leaders_eqr : int -> t -> nodeSetSet
  val k_followers_eqr : int -> t -> nodeSetSet
  val jk_nhoods_eqr : int -> int -> t -> nodeSetSet
(*   val ngrams : int -> t -> nodeSetSet *)
(* 
   val lda -> t -> nodeSetSet
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
  (D:Delim.DELIM_TYPE) =

struct

  let name = "FSA"
  type node = Node.t
  type edge = Edge.t
  type label = Symbol.t
  type vocab = Vocab.t
  type word = Word.t
  type wordSet = WordSet.t
  type symbol = Symbol.t
  type nodeSetSet = NodeSetSet.t

  module FSM = Machines.Make(Node)(NodeSet)(Edge)(EdgeSet)(D)
  include FSM


  (* 
     TOOLS
     All the major functions below use one or more of the following modules 
  *)

  module OrderedNodePairs = 
  struct 
    type t = Node.t * Node.t 
    let compare (a,b) (c,d) = 
      let r = Node.compare a c in
      if r = 0 then Node.compare b d else r
  end
    
  module NxNSet = Set.Make(OrderedNodePairs)

  (* this makes a set of all node pairs (x,y) where x <> y *)
  (* It is used in (prefix) tree construction where we need to 
     check if transition from x goes to y and also from y to x. *)
  let make_nxnset ns = 
    Mset.cross NxNSet.add 
      (fun x y-> if x=y then raise (Failure("NonCombinable")) else (x,y))
      NodeSet.fold NxNSet.empty ns ns;;

  (* This make a set of all node pairs (x,y) such that x <> y and such that
     (y,x) is not in the set. It is used when we make equivalence classes
     among the nodes -- if we know (a,b) are equivalent we do not need to
     check to see if (b,a) is also equivalent. *)

  let make_nxnset_eqr ns =
    let nss = NodeSet.fold
      (fun n1 nss1 -> NodeSet.fold
	(fun n2 nss2 ->
	  if n1 = n2 then nss2 else 
	    NodeSetSet.add (NodeSet.add n1 (NodeSet.singleton n2)) nss2
	) ns nss1 
      ) ns NodeSetSet.empty
    in
    NodeSetSet.fold
      (fun ns nxnset -> NxNSet.add (NodeSet.min_elt ns,NodeSet.max_elt ns) nxnset)
      nss
      NxNSet.empty


  (* Sets of node sets provide partitions of nodesets. *)
  module NodeMap = Map.Make(Node)
  module LabelSet = Set.Make(Symbol)

  (* we use the edgemap like this-- EdgeMap: (node,label) -> nodeset *)
  module OrderedNodeLabelPairs =
  struct 
    type t = Node.t * Edge.label 
    let compare (n1,a1) (n2,a2) = 
      let r = Node.compare n1 n2 in
      if r = 0 then Edge.label_compare a1 a2 else r
  end

  module EdgeMap = Map.Make(OrderedNodeLabelPairs)

  type edgeMap = nodeSet EdgeMap.t 

  let edgeMap_find = EdgeMap.find

  let origin = Edge.origin
  let terminus = Edge.terminus 

  let edgeSet_to_edgeMap es =
    EdgeSet.fold 
      (fun e map -> 
	let o = origin e and l = Edge.label e in
	try let ns = EdgeMap.find (o,l) map in
	    EdgeMap.add (o,l) (NodeSet.add (terminus e) ns) map
	with Not_found -> 
	  EdgeMap.add (o,l) (NodeSet.singleton (terminus e)) map  )
      es
      EdgeMap.empty

  let edgeMap_to_edgeSet em =
    EdgeMap.fold
    (fun (o,l) d es1 -> NodeSet.fold 
      (fun n es2 -> EdgeSet.add (Edge.make o n l) es2)
      d es1
    ) em EdgeSet.empty

  let print_edgeMap t =
    let em = (edgeSet_to_edgeMap (edges t)) in
    EdgeMap.iter (fun (o,l) ns ->
      print_string (Node.to_string o); print_string " * ";
      print_string (Symbol.to_string l); print_string " -> ";
      print_string (NodeSet.to_string ns); print_endline "")
      em

  (* usage (e.g.): edgeSet_to_nodeSet origin es *)
  let edgeSet_to_nodeSet f es = 
    EdgeSet.fold 
      (fun e ns -> NodeSet.add (f e) ns)
      es
      NodeSet.empty

  let edgeSet_to_labelSet es = 
    EdgeSet.fold (fun e ls -> LabelSet.add (Edge.label e) ls) es LabelSet.empty

  let word_to_labelSet word = 
    Word.fold_left (fun ls s -> LabelSet.add (Symbol.of_x s) ls) LabelSet.empty word


  (* MERGING STATES ACCORDING TO SOME PARTITION *)

  (* Sometimes (as in minimize) we get a set of nodepairs which are
     equivalent. So here we convert it to a partition. *)

  (* we use NodeSetSet for all kinds of things, especially partitions! *)    

  let trivial_partition ns = 
    NodeSet.fold (fun n nss -> NodeSetSet.add (NodeSet.singleton n) nss)
      ns NodeSetSet.empty

  let equiv_nodes_of n nxnset = 
    NxNSet.fold (fun (x,y) ns -> 
      let is_x,is_y = n=x,n=y in match is_x,is_y with
	| true,false -> NodeSet.add y ns
	| false,true -> NodeSet.add x ns
	| _ -> ns)
      nxnset (NodeSet.singleton n)

  (* we give ourselves nodes only to make the computation easier *)
  let nxnSet_to_partition nodes nxnset = NodeSet.fold 
    (fun n nss -> NodeSetSet.add (equiv_nodes_of n nxnset) nss)
    nodes
    NodeSetSet.empty

  (* the new node is just the pairing of the nodes in a block *)
  let merge_nodeset ns = 
    if NodeSet.is_empty ns then failwith "Acceptors.merge_nodeset: Empty Nodeset"
    else if NodeSet.cardinal ns = 1 then NodeSet.choose ns
    else NodeSet.fold
      (fun n newnode -> Node.pair n newnode)
      (NodeSet.remove (NodeSet.min_elt ns) ns) 
      (NodeSet.min_elt ns)

  (* converts a partition to the new node set *)
  let nodeSetSet_to_nodeSet nss = 
    NodeSetSet.fold (fun nodeset new_ns ->
      NodeSet.add (merge_nodeset nodeset) new_ns)
      nss NodeSet.empty

  (* the node map (node -> block) used here is just an easy way to keep track
     of the partition module NodeMap = Map.Make(Node) *)

  let partition_to_map nss = 
    NodeSetSet.fold (fun set map ->
      NodeSet.fold (fun node m -> 
	NodeMap.add node set m)
	set
	map)
      nss NodeMap.empty

  let partition_edge edge piMap =
    Edge.make 
      (merge_nodeset (NodeMap.find (Edge.origin edge) piMap))
      (merge_nodeset (NodeMap.find (Edge.terminus edge) piMap))
      (Edge.label edge)

  let merge1 fsa partition = 
    (*    if not (Mset.is_partition partition (nodes fsa)) then 
	  failwith "Supplied set of nodesets does not partition the FSA"
	  else*)
      let piMap = partition_to_map partition in
      let new_nodeset nodeset pi = 
	let nss = NodeSetSet.filter (fun nset -> 
	  not (NodeSet.is_empty (NodeSet.inter nodeset nset))) 
	  pi 
	in
	nodeSetSet_to_nodeSet nss 
      in
      let new_starts = new_nodeset (starts fsa) partition in
      let new_finals = new_nodeset (finals fsa) partition in
      let new_edges = EdgeSet.fold (fun x es -> 
	EdgeSet.add (partition_edge x piMap) es)
	(edges fsa) EdgeSet.empty
      in
      make new_starts new_finals new_edges
	
  (* returns a machine where only one set of nodes has been merged.*)
  let merge1_ns fsa ns =
    let nodes = nodes fsa in
    let trivial_sub_pi = trivial_partition (NodeSet.diff nodes ns) in
    let pi = NodeSetSet.add ns trivial_sub_pi in
    merge1 fsa pi
    
  let rec merge fsa eqr =
    let tpi = trivial_partition (nodes fsa) in
    let pi_induced_by_eqr = eqr fsa in
    if NodeSetSet.equal tpi pi_induced_by_eqr then fsa
    else merge (merge1 fsa pi_induced_by_eqr) eqr

    



  (* DETERMINISTIFY *)


  let epsilon_nodeset ns edgemap =
    (* finds the epsilon closure of a nodeset *)
    let rec helper new_nodes old_nodes =
      if NodeSet.subset new_nodes old_nodes then old_nodes
      else
	let next_nodes = NodeSet.fold
	  (fun n new_ns -> try
	      NodeSet.union new_ns (edgeMap_find (n,Symbol.blank) edgemap)
	    with Not_found -> new_ns)	  
	  new_nodes NodeSet.empty
	in
	helper next_nodes new_nodes
    in
    if NodeSet.is_empty ns then ns
    else NodeSet.union ns (helper ns NodeSet.empty)

  let complete_determinize m = 
    let alphabet = edgeSet_to_labelSet (edges m) in
    let	edgesM = edges m and finalsM = finals m 
    and startsM = starts m in
    let edgeMapM = edgeSet_to_edgeMap edgesM in
    let null = Node.of_string "<NULL>" in
    let rec helper to_do_nodesets checked_nodesets map finalset = 
      if NodeSetSet.is_empty to_do_nodesets                                           (* if there are no more nodesets to pursue, quit *)
      then map,finalset
      else 
	let ns =  (NodeSetSet.choose to_do_nodesets) in                                (* get a nodeset to determinize *)
	let newNode = merge_nodeset ns in                                              (* merge the nodeset to a new node *)
	let newmap,new_to_do_nss = LabelSet.fold                                       (* fold through the alphabet *)
	  (fun a (map,to_do_nss) ->                                                         (* ignore epsilon transitions *)
	    if a = Symbol.blank 
	    then (map,to_do_nss)
	    else
	      let termini = edgeSet_to_nodeSet terminus                                      (* find all reachable nodes (termini) from nodeset *) 
		(EdgeSet.filter (fun e ->                                                       (* with symbol 'a' *)
		  NodeSet.mem (origin e) ns 
		  && (Edge.label e) = a) edgesM)
	      in
	      let term,ns_term = if NodeSet.is_empty termini                                 (* if there are none then go to null from newnode with 'a' *)
		then null, NodeSet.singleton null
		else 
		  let ens_termini = epsilon_nodeset termini edgeMapM in                      (* else let the terminus node be the merged epsilon *) 
		  (merge_nodeset (ens_termini)), ens_termini                                   (* closure of termini and let ns_term be the non-merged *) 
	      in                                                                               (* epsilon closure of termini *)   
	      (EdgeMap.add (newNode,a) (NodeSet.singleton term) map, 
	      NodeSetSet.add ns_term to_do_nss)
	  ) alphabet (map,to_do_nodesets)
	in
	let new_finals =                                                                (* if any of the nodes in the node set were finals *)
	  if NodeSet.is_empty (NodeSet.inter finalsM ns)                                 (* then the new node is final too *)
	  then finalset else NodeSet.add newNode finalset
	in
	let new_checked_ns = NodeSetSet.add ns checked_nodesets in                      (*  update the checked nodesets *)
	helper (NodeSetSet.diff new_to_do_nss checked_nodesets) 
	  new_checked_ns newmap new_finals
    in
    let newstarts = epsilon_nodeset startsM edgeMapM in
    let deterministic_edgemap,new_finals = helper                              (* make the deterministic edgemap and newfinals *)
      (NodeSetSet.singleton newstarts)
      NodeSetSet.empty EdgeMap.empty NodeSet.empty
    in
    make (NodeSet.singleton (merge_nodeset newstarts))                         (* make the deterministic FSA *)
      new_finals (edgeMap_to_edgeSet deterministic_edgemap)


  let determinize m = trim (complete_determinize m)

  let is_deterministic m = 
    let edgemap = edgeSet_to_edgeMap (edges m) in
    NodeSet.cardinal (starts m) = 1 &&
	EdgeMap.fold (fun k d bool -> 
	  if NodeSet.cardinal d = 1 then true else false 
	    && bool ) edgemap true

  (* MINIMIZE *)

  (* two states are distinguishable iff 
     1. they are not both final or both nonfinal (i.e. one has a good tail
     \lambda but not the other)
     2. they are b-predecessors of distinguishable states *)      

  let basis fset nxnSet = 
    let are_distinguishable n1 n2 =
      ((NodeSet.mem n1 fset) && (not (NodeSet.mem n2 fset)))
      || ((NodeSet.mem n2 fset) && (not (NodeSet.mem n1 fset)))
    in
    NxNSet.filter (fun (x,y) -> are_distinguishable x y) nxnSet

  let distinguish_b_preds ds nxnset labelsM edgeMap =
    let rec helper distinguishables nodepairs_to_check =
      let newly_dists = LabelSet.fold 
	(fun l nxns1 ->
	  NxNSet.union nxns1
	    (NxNSet.fold 
	      (fun (p,q) nxns2 -> 
		(* here is where we use the fact that 
		   m is complete and deterministic *)
		  let r = NodeSet.choose (EdgeMap.find (p,l) edgeMap) and
		      s = NodeSet.choose (EdgeMap.find (q,l) edgeMap) in
		  if NxNSet.mem (r,s) distinguishables 
		  then NxNSet.add (p,q) nxns2
		  else nxns2
	      ) nodepairs_to_check nxns1
	    )
	) labelsM NxNSet.empty
      in
      if NxNSet.is_empty (NxNSet.diff newly_dists distinguishables)
      then distinguishables
      else helper (NxNSet.union newly_dists distinguishables)
	(NxNSet.diff nodepairs_to_check newly_dists)
    in
    helper ds nxnset

  let minimize mchn = 
    let m = complete_determinize mchn in
    let nodesM = nodes m and finalsM = finals m and edgesM = edges m in
    let edgeMap = edgeSet_to_edgeMap edgesM in
    let	labelsM = edgeSet_to_labelSet edgesM in
    let nxnSet = make_nxnset nodesM in
    let base = basis finalsM nxnSet in
    let dist_nodes = 
      distinguish_b_preds base (NxNSet.diff nxnSet base) labelsM edgeMap
    in
    let equiv_nodes = NxNSet.diff nxnSet dist_nodes in
    let pi = nxnSet_to_partition nodesM equiv_nodes in
    trim (merge1 m pi)


  let complement m = 
    let det_m = complete_determinize m in
    let nonfinals = NodeSet.diff (nodes det_m) (finals m) in
    let cmp = make (starts det_m) nonfinals (edges det_m) in
    FSM.rename (FSM.trim cmp)

  (* PREFIX TREE CONSRUCTION *)

  (*
    let rec extra_letter w1 w2 = 
    match (w1,w2) with
    | ([],a::[]) -> a
    | (h1::t1,h2::t2) when h1=h2 -> extra_letter t1 t2
    | _ -> raise (Failure("Not same plus one letter"));;

    We have to write extra_letter as below because we have implemented lists as
    abstract types so the match with command does not work.
  *)

  let rec extra_letter w1 w2 = 
    if w2 = Word.empty
    then raise (Failure("Acceptors.extra_letter: Not same plus one letter"))
    else if (w1 = Word.empty) && (Word.tl w2 = Word.empty) 
    then Word.hd w2
    else if (Word.hd w1) = (Word.hd w2) 
    then extra_letter (Word.tl w1) (Word.tl w2)
    else raise (Failure("Acceptors.extra_letter: Not same plus one letter"))


  let wordSet_to_nodeSet ws = 
    WordSet.fold (fun word ns -> 
      NodeSet.add (Node.of_string (Word.to_string word)) ns)
      ws
      NodeSet.empty

  (* we use NxN Sets to build edges *)

  let pt_edges nxnset = 
    NxNSet.fold (fun (x,y) es ->
      let wx = Word.of_string (Node.to_string x)
      and wy = Word.of_string (Node.to_string y) in 
      try let a = extra_letter wx wy in
	  EdgeSet.add (Edge.make x y (Symbol.of_x a)) es
      with _ -> es  )
      nxnset
      EdgeSet.empty

  let extend_pt pt word = 
    let prefixes = List.fold_left 
      (fun ws p -> WordSet.add p ws)
      WordSet.empty
      (Word.prefixes word)
    in
    let nxnset = make_nxnset 
      (wordSet_to_nodeSet prefixes) 
    in
    make (starts pt) 
      (NodeSet.add (Node.of_string (Word.to_string word)) (finals pt))
      (EdgeSet.union (edges pt) (pt_edges nxnset))

  let pt wordset = 
    let startset = NodeSet.singleton (Node.of_string Word.empty_string) in
    WordSet.fold
      (fun w pt -> extend_pt pt w)
      wordset
      (make startset NodeSet.empty EdgeSet.empty)

  let st wordset = 
    let rev_ws =  WordSet.fold
      (fun w ws -> WordSet.add (Word.rev w) ws) 
      wordset
      WordSet.empty
    in
    reverse (pt rev_ws)

  let wordset_of_stringlist sl =
    List.fold_left 
      (fun ws s -> WordSet.add (Word.of_string s) ws) 
      WordSet.empty 
      sl

  let tree_maker pt_type filename =
    let wordset = wordset_of_stringlist
      (File.to_stringlist filename)
    in 
    pt_type wordset

  let make_pt filename = tree_maker pt filename
  let make_st filename = tree_maker st filename

  let wordSet_of_string = WordSet.of_string


(* RANGE FSAs *)

  module VocabSet = Xset.Make(Delim.Dash_no_bs)(Vocab)

  let vocabSet_of_word w = 
    Word.fold_left (fun vs v -> VocabSet.add v vs)
      VocabSet.empty
      w

  let node_of_vocabSet vs =
    Node.of_string (VocabSet.to_string vs)

  let wordSet_to_nodeSet_range ws = 
    WordSet.fold (fun word ns -> 
      NodeSet.add (node_of_vocabSet (vocabSet_of_word word)) ns)
      ws
      NodeSet.empty

  let rec extra_letter_range vs1 vs2 = 
    let diff = VocabSet.diff vs2 vs1 in
    if VocabSet.cardinal diff <> 1
    then raise (Failure("Acceptors.extra_letter_range: Not same plus one letter"))
    else VocabSet.choose diff

  let node_to_string_range node = 
    let s = Node.to_string node in
    if s = "" then "<emptyset>" else s

  let range_fsa_edges nxnset = 
    NxNSet.fold (fun (x,y) es ->
      let vsx = VocabSet.of_string (node_to_string_range x)
      and vsy = VocabSet.of_string (node_to_string_range y) in 
      try let a = extra_letter_range vsx vsy in
      let es1 = EdgeSet.add (Edge.make x y (Symbol.of_x a)) es in
      VocabSet.fold (fun v es ->
	EdgeSet.add (Edge.make y y (Symbol.of_x v)) es)
	vsy es1
      with _ -> es  )
      nxnset
      EdgeSet.empty


  let extend_range_fsa rt word = 
    let prefixes = List.fold_left 
      (fun ws p -> WordSet.add p ws)
      WordSet.empty
      (Word.prefixes word)
    in
    let nxnset = make_nxnset (wordSet_to_nodeSet_range prefixes) in
    make (starts rt) 
      (NodeSet.add (node_of_vocabSet (vocabSet_of_word word)) (finals rt))
      (EdgeSet.union (edges rt) (range_fsa_edges nxnset))

  let range_fsa wordset = 
    let startset = NodeSet.singleton 
      (Node.of_string (VocabSet.to_string VocabSet.empty)) 
    in
    WordSet.fold
      (fun w rt -> extend_range_fsa rt w)
      wordset
      (make startset NodeSet.empty EdgeSet.empty)

  let make_range_fsa filename = tree_maker range_fsa filename

  (* OPERATIONS *)      

  let node_of_int x = Node.of_string (string_of_int x)

  let make_eps_from_ns1_to_ns2 ns1 ns2 =
    NodeSet.fold 
      (fun n1 es1 -> NodeSet.fold
	(fun n2 es2 -> EdgeSet.add
	  (Edge.make n1 n2 Symbol.blank) es2)
	ns2 es1
      )
      ns1 EdgeSet.empty

  let concat fsa1 fsa2 = 
    let r1 = rename fsa1 in
    let size1 = NodeSet.cardinal (nodes r1) in
    let r2 = rename_n size1 fsa2 in
    let eps_edges = 
      make_eps_from_ns1_to_ns2 (finals r1) (starts r2)
    in
    make 
      (starts r1) (finals r2)
      (EdgeSet.union (edges r1)
	(EdgeSet.union (edges r2) eps_edges))

  let star fsa = 
    let r1 = rename_n 1 fsa in
    let start = node_of_int 0 in
    let eps_edges1 = NodeSet.fold 
      (fun n es -> EdgeSet.add 
	(Edge.make start n Symbol.blank) es
      ) (starts r1) EdgeSet.empty
    in
    let final = node_of_int ((NodeSet.cardinal (nodes r1))+1) in
    let eps_edges2 = NodeSet.fold 
      (fun n es -> (EdgeSet.add (Edge.make n final Symbol.blank) es))
      (finals r1) EdgeSet.empty
    in
    let eps_edges3 = 
      make_eps_from_ns1_to_ns2 (finals r1) (starts r1)
    in
    let eps_edges = EdgeSet.union eps_edges1
      (EdgeSet.union eps_edges2 eps_edges3)
    in
    make 
      (NodeSet.singleton start)
      (NodeSet.singleton final)
      (EdgeSet.add
	(Edge.make start final Symbol.blank)
	(EdgeSet.union (edges r1) (eps_edges)))

  (* ACCEPTING WORDS *)

  exception No_path of word * nodeSet

  let transform fsa nodeset word =
    let edgeMap = edgeSet_to_edgeMap (edges fsa) in
    let rec process w ns =
      if w = Word.empty then ns
      else
	let s = Symbol.of_x (Word.hd w) in 
	let possible_current_nodes = 
	  epsilon_nodeset ns edgeMap
	in
	let nodeset_with_s = NodeSet.fold 
	  (fun n new_ns -> try
	      NodeSet.union new_ns (edgeMap_find (n,s) edgeMap)
	    with Not_found -> new_ns)
	  possible_current_nodes NodeSet.empty
	in
	let nodeset_with_wild = NodeSet.fold
	  (fun n new_ns -> try
	      (NodeSet.union new_ns (edgeMap_find (n,Symbol.wild) edgeMap))
	    with Not_found -> new_ns)
	  possible_current_nodes NodeSet.empty
	in
	let next_nodeset = NodeSet.union nodeset_with_s nodeset_with_wild in
	if NodeSet.is_empty next_nodeset 
	then raise (No_path(w,ns))
	else process (Word.tl w) next_nodeset 
    in
    process word nodeset

  let accepts_word fsa word  = 
    try let nodeset = transform fsa (starts fsa) word in
	not (NodeSet.is_empty (NodeSet.inter (finals fsa) nodeset))
    with No_path(_,_) -> false

  let accepts fsa s = 
    let word = Word.of_string s in
    accepts_word fsa word 

  let accepts_wordSet fsa wordset = 
    WordSet.partition (accepts_word fsa) wordset


  (* EXTENDING FSAs.  *)


  (* This is like prefix tree construction except here the
     nodes need not be named based on the sample. I.e. this works for any FSA
     we want to extend. Note that if the FSA is not deterministic, this
     function will choose its own path through the machine. It assumes that
     the FSA has been renamed. *)

  let add_word_to_fsa fsa node word = 
    let num_nodes = NodeSet.cardinal (nodes fsa) in
    let rec process i w es = 
      if w = Word.empty then es,i
      else 
	let a = Word.hd w and t = Word.tl w in
	let new_edge = 
	  Edge.make (node_of_int i) (node_of_int (i+1)) (Symbol.of_x a)
	in
	process (i+1) t (EdgeSet.add new_edge es)
    in
    if word = Word.empty 
    then 
      make (starts fsa) 
	(NodeSet.add (NodeSet.choose (starts fsa)) (finals fsa)) 
	(edges fsa) 
    else
      let new_edges,new_final = process (num_nodes) (Word.tl word)
	(EdgeSet.add 
	  (Edge.make node (node_of_int num_nodes) (Symbol.of_x (Word.hd word)))
	  EdgeSet.empty)
      in
      make (starts fsa) 
	(NodeSet.add (node_of_int (new_final)) (finals fsa))
	(EdgeSet.union new_edges (edges fsa))
	
	
  let extend fsa word = 
    try let ns = transform fsa (starts fsa) word in
	make (starts fsa) 
	  (NodeSet.add (NodeSet.choose ns) (finals fsa))
	  (edges fsa)
    with (No_path(w,ns)) -> add_word_to_fsa fsa (NodeSet.choose ns) w


  let pt_maker2 file f = 
    let ic = open_in file in
    let t = File.fold f ic empty in
    let () = close_in ic in
    t

  let make_pt2 file =   
    let f s t = extend t (Word.of_string s) in
    pt_maker2 file f

  let make_st2 file =   
    let f s t = extend t (Word.rev (Word.of_string s)) in
    reverse (pt_maker2 file f)


  let make_fin ic = 
    (* a nondeterministic representation of the input where
       each word is its own branch off of the root node *)
    let start = NodeSet.choose (starts empty) in
    let f s t = (union (add_word_to_fsa empty start (Word.of_string s)) t) in
    File.fold f ic empty


  let sigma_edges node1 node2 alph = 
    (* this funtion adds edges from node1 to node2 with all symbols in alph to node *)
    LabelSet.fold
      (fun l es -> EdgeSet.add (Edge.make node1 node2 l) es)
      alph
      EdgeSet.empty

  let shuffle_ideal alphabet_word word =
    (* builds a machine which accepts exactly the shuffle ideal of word *)
    let alphabet = word_to_labelSet alphabet_word in
    let wordfsa = extend empty word in
    let edgeset = 
      NodeSet.fold
	(fun n es1 -> 
	   let es = sigma_edges n n alphabet in
	   EdgeSet.union es es1
	)
	(nodes wordfsa)
	(edges wordfsa)
    in
    rename (minimize (make (starts wordfsa) (finals wordfsa) edgeset))

  let sigma_star alphabet_word = 
    let alphabet = word_to_labelSet alphabet_word in
    let state =  (Node.of_string "0") in
    let edgeset = sigma_edges state state alphabet in
    make (NodeSet.singleton state) (NodeSet.singleton state) edgeset

  let factor_ideal alphabet_word word =
    (* builds a machine which accepts exactly the language where every
       word contains the factor word *)
    let wordfsa = extend empty word in
    let sig_star = sigma_star alphabet_word in
    let fsa_with_eps = concat sig_star (concat wordfsa sig_star) in
    rename (minimize fsa_with_eps)

  (* GENERATING WORDS and K LEADERS and K FOLLOWERS *)

  module Path = Xlist.Make(Delim.Point_abs)(Edge)
  module PathSet = Xset.Make(Delim.Newline_no_bs)(Path)

  (* paths are built in reverse so the end of the path is the list head *)
  let new_paths path node edges = 
    let edges_from = EdgeSet.filter 
      (fun x -> Edge.origin x = node)
      edges
    in
    EdgeSet.fold
      (fun e ps -> PathSet.add (Path.build e path) ps)
      edges_from
      PathSet.empty
      
  let extend_paths pathset edges = 
    PathSet.fold
      (fun p new_ps ->
	let current_node = Edge.terminus (Path.hd p) in      
	let new_pathset = new_paths p current_node edges in
	PathSet.union new_pathset new_ps
      )
      pathset
      PathSet.empty


  (* find paths of length k which depart from the nodes in nodeset *)
  let paths_of_length_k k nodeset edges = 
    let rec helper m =
      match m with
	| 0 -> PathSet.singleton (Path.empty)
	| 1 -> NodeSet.fold 
	    (fun n ps -> PathSet.union ps (new_paths Path.empty n edges))
	      nodeset	      
	      PathSet.empty
	| _ -> extend_paths (helper (m-1)) edges
    in
    helper k

  let path_to_word p = 
    Path.fold_left 
      (fun w e -> 
	let s = Edge.label e in 
	if s = Symbol.blank then w
	else if s = Symbol.wild 
	then failwith "Acceptor.path_to_word: converting paths to words with universal symbols not implemented"
	else Word.build (Symbol.to_x s) w 
      ) Word.empty p

  (* returns all accepted words that occur along some path *)
  let path_to_wordSet p finals = 
    let rec helper p ws =
      if p = Path.empty then ws 
      else if NodeSet.mem (Edge.terminus (Path.hd p)) finals 
      then WordSet.add (path_to_word p) (helper (Path.tl p) ws)
      else helper (Path.tl p) ws
    in
    helper p WordSet.empty


  let rec paths_up_to_k k num nodeset edgeset = 
    if num > k then PathSet.empty
    else PathSet.union 
      (paths_of_length_k num nodeset edgeset) 
      (paths_up_to_k k (num+1) nodeset edgeset) 

  let generate fsa min max = 
    let startsM = starts fsa and finalsM = finals fsa in
    let pathset = paths_up_to_k max min startsM (edges fsa) in
    let initial_wordset = 
      if min = 0 && not (NodeSet.is_empty (NodeSet.inter startsM finalsM))
      then WordSet.singleton (Word.empty)
      else WordSet.empty 
    in
    PathSet.fold 
      (fun p ws -> 
	if p = Path.empty then ws
	else if NodeSet.mem (Edge.terminus (Path.hd p)) finalsM
	then WordSet.add (path_to_word p) ws
	else ws) 
      pathset 
      initial_wordset

  (* generate and print *)
  let generate_p fsa min max = 
    let startsM = starts fsa and finalsM = finals fsa in
    let pathset = paths_up_to_k max min startsM (edges fsa) in
    let () =   (* prints the empty string if recognized by fsa *)
      if min = 0 && not (NodeSet.is_empty (NodeSet.inter startsM finalsM))
      then Word.print Word.empty 
      else () 
    in
    PathSet.iter
      (fun p -> 
	if p = Path.empty then ()
	else if NodeSet.mem (Edge.terminus (Path.hd p)) finalsM
	then Word.print (path_to_word p))
      pathset

  let k_followers k nodeset edgeset = 
    let pathset = paths_up_to_k k 0 nodeset edgeset in
    PathSet.fold (fun p ws -> WordSet.add (path_to_word p) ws) 
      pathset
      WordSet.empty

  let reverse_wordSet ws = WordSet.fold
    (fun w ws -> WordSet.add (Word.rev w) ws)
    ws WordSet.empty

  let k_leaders k nodeset edgeset =
    let rev_edges = EdgeSet.fold (fun e es -> 
      EdgeSet.add (Edge.make (terminus e) (origin e) (Edge.label e)) es)
      edgeset EdgeSet.empty
    in
    reverse_wordSet (k_followers k nodeset rev_edges)

  let print_wordSet = WordSet.print 

  type neighborhood = {incoming:wordSet;outgoing:wordSet;final:bool;start:bool}

  let jk_nhood j k state edges finals starts = 
    {incoming = k_leaders j (NodeSet.singleton state) edges;
     outgoing = k_followers k (NodeSet.singleton state) edges;
     final = NodeSet.mem state finals;
     start = NodeSet.mem state starts
    }

(* some utilities for debugging *)
   
  let print_hood h = print_endline (Mstring.concat_list " : " 
   [WordSet.to_string h.incoming; WordSet.to_string h.outgoing; string_of_bool h.final; string_of_bool h.start] "")

  let print_nhoodmap map = NodeMap.iter (fun k d -> Node.print_ k; print_string "\t";
    print_hood d) map


  (* HERE WE DEFINE SOME OTHER USEFUL EQUIVALENCE RELATIONS FOR MERGING! *)

  (* in this implementation, an equivalence relation should take a machine as
     input and return a partition of nodes; i.e. a nodeSetSet. Note that if
     getting a set of equivalent node pairs is easier, we can convert that to a
     partition as in minimize. *)

  let equiv_nodes_of n nxnset = 
    NxNSet.fold (fun (x,y) ns -> 
      let is_x,is_y = n=x,n=y in match is_x,is_y with
	| true,false -> NodeSet.add y ns
	| false,true -> NodeSet.add x ns
	| _ -> ns)
      nxnset (NodeSet.singleton n)

  let nxnSet_to_partition nodes nxnset = NodeSet.fold 
    (fun n nss -> NodeSetSet.add (equiv_nodes_of n nxnset) nss)
    nodes
    NodeSetSet.empty

  (* Zero reversible languages *)
  let b_successors m = 
    let finalsM = finals m  and nodesM = nodes m in
    if NodeSet.cardinal finalsM > 1 then 
      NodeSetSet.add finalsM 
	(trivial_partition (NodeSet.diff nodesM finalsM))
    else
      let edgesM = edges m in
      let labelsM = edgeSet_to_labelSet edgesM in
      let edgeMap = edgeSet_to_edgeMap edgesM in
      let nxnset = make_nxnset_eqr nodesM in
      let equiv_nodes = 
	NxNSet.fold 
	  (fun (x,y) eq_nxns->
	    let have_b_successor = LabelSet.fold
	      (fun l bool -> bool 
		|| try let nsx = edgeMap_find (x,l) edgeMap in
		let nsy = edgeMap_find (y,l) edgeMap in
		not (NodeSet.is_empty (NodeSet.inter nsx nsy))
		  with Not_found -> false)
	      labelsM false
	    in
	    if have_b_successor then NxNSet.add (x,y) eq_nxns
	    else eq_nxns
	  )
	  nxnset
	  NxNSet.empty
      in
      nxnSet_to_partition nodesM equiv_nodes


  (* find equiv nodes takes the set of nodes (all states in the machine) a
     nodemap (which maps nodes to values) an equivalence relation (which
     determines whether two nodes are equivalent on the basis of their values)
     and a "partition" (this is updated recursively and should be an empty
     nodesetset-- it will become the partition returned by this function)

     This function returns a partition of the nodeset according to the
     equivalence relation
  *)

  let rec find_equiv_nodes nodeset nodemap equiv pi =
    if NodeSet.is_empty nodeset then pi
    else
      let n = NodeSet.choose nodeset in
      let n_value = NodeMap.find n nodemap in
      let any_eq_nodes,next_nodemap1 = NodeMap.fold 
	(fun k d (eq_nodes,next_nm) -> 
	  if equiv n_value d then
	    (NodeSet.add k eq_nodes, NodeMap.remove k next_nm)
	  else eq_nodes,next_nm
	) nodemap (NodeSet.empty,nodemap)
      in
      (* if there are no equivalent nodes for n in nodemap then n is its own block *)
      let equiv_nodes,next_nodemap2 = 
	if NodeSet.is_empty any_eq_nodes 
	then (NodeSet.singleton n),NodeMap.remove n next_nodemap1
	else any_eq_nodes,next_nodemap1
      in
      find_equiv_nodes 
	(NodeSet.diff nodeset equiv_nodes)
	next_nodemap2
	equiv 
	(NodeSetSet.add equiv_nodes pi)
      

  (* These functions are equivalence relations which use find_equiv_nodes to
     return a partition over states in a machine *)
	
  let is_final_eqr m = 
    let nodesM = nodes m in
    let finalsM = finals m in
    let map = NodeSet.fold 
      (fun n nm -> NodeMap.add n (NodeSet.mem n finalsM) nm)
      nodesM NodeMap.empty
    in
    let equiv x y = x && y
    in
    find_equiv_nodes nodesM map equiv NodeSetSet.empty


  let is_nonfinal_eqr m = 
    let nodesM = nodes m in
    let finalsM = finals m in
    let map = NodeSet.fold 
      (fun n nm -> NodeMap.add n (NodeSet.mem n finalsM) nm)
      nodesM NodeMap.empty
    in
    let equiv x y = (not x) && (not y)
    in
    find_equiv_nodes nodesM map equiv NodeSetSet.empty


  let is_start_eqr m = 
    let nodesM = nodes m in
    let startsM = starts m in
    let map = NodeSet.fold 
      (fun n nm -> NodeMap.add n (NodeSet.mem n startsM) nm)
      nodesM NodeMap.empty
    in
    let equiv x y = x && y
    in
    find_equiv_nodes nodesM map equiv NodeSetSet.empty


  let is_nonstart_eqr m = 
    let nodesM = nodes m in
    let startsM = starts m in
    let map = NodeSet.fold 
      (fun n nm -> NodeMap.add n (NodeSet.mem n startsM) nm)
      nodesM NodeMap.empty
    in
    let equiv x y = (not x) && (not y)
    in
    find_equiv_nodes nodesM map equiv NodeSetSet.empty


  let k_followers_eqr k m =
    let edgesM = edges m in
    let nodesM = nodes m in 
    let map = NodeSet.fold 
      (fun n nm -> NodeMap.add n 
	(k_followers k (NodeSet.singleton n) edgesM) nm)
      nodesM NodeMap.empty
    in
    let equiv x y = WordSet.equal x y 
    in
    find_equiv_nodes nodesM map equiv NodeSetSet.empty

  let k_leaders_eqr k m = 
    let edgesM = edges m in
    let nodesM = nodes m in
    let map = NodeSet.fold 
      (fun n nm -> NodeMap.add n 
	(k_leaders k (NodeSet.singleton n) edgesM) nm)
      nodesM NodeMap.empty
    in
    let equiv x y = WordSet.equal x y 
    in
    find_equiv_nodes nodesM map equiv NodeSetSet.empty


  let jk_nhoods_eqr j k m =
    let finalsM = finals m and nodesM = nodes m and 
	startsM = starts m and edgesM = edges m in
    let map = NodeSet.fold 
      (fun n nm -> NodeMap.add n (jk_nhood j k n edgesM finalsM startsM) nm)
      nodesM
      NodeMap.empty 
    in
    let equiv nh1 nh2 = WordSet.equal nh1.incoming nh2.incoming
      && WordSet.equal nh1.outgoing nh2.outgoing 
      && nh1.final = nh2.final 
      && nh1.start = nh2.start
    in
    find_equiv_nodes nodesM map equiv NodeSetSet.empty


  let is_eqr_distinct eqr m = 
    let nodesM = nodes m in
    let pi = eqr m in 
    NodeSetSet.cardinal pi = NodeSet.cardinal nodesM

  exception Not_nd

  let is_nd j k m = 
    let finalsM = finals m and nodesM = nodes m and 
	startsM = starts m and edgesM = edges m in
    let map = NodeSet.fold 
      (fun n nm -> 
	NodeMap.add n (jk_nhood j k n edgesM finalsM startsM) nm)
      nodesM
      NodeMap.empty 
    in
    let unique_nhood node m =
      let nhood = NodeMap.find node m in
      let m2 = NodeMap.remove node m in
      let _ = 
	NodeMap.fold
	  (fun k d b -> 
	    if d=nhood then raise Not_nd
	    else true
	  ) m2 true
      in
      m2
    in
    try 
      let _ = NodeSet.fold
	(fun n m -> unique_nhood n m) nodesM map
      in 
      true
    with Not_nd -> false
      
  let ngrams n m = k_leaders_eqr (n-1) m


end





 (* here are some quick (Q) acceptor functors which take fewer arguments *)
module Make_Q
  (Vocab:Symbols.SYMBOL_SEED) 
  (Node:X.X_TYPE) =

struct
  module Word = Xlist.Make(Delim.None)(Vocab);;
  module WordSet = Xset.Make(Delim.Newline_no_bs)(Word)
  module Symbol = Symbols.Make(Vocab);;
  module NodeSet = Xset.Make(Delim.Comma_no_bs)(Node)
  module NodeSetSet = Xset.Make(Delim.Newline_no_bs)(NodeSet)
  module Edge = Edges.Make(Node)(Symbol)(Delim.Comma_no_bs)
  module EdgeSet = Xset.Make(Delim.Newline_no_bs)(Edge)
  module A = Make(Vocab)(Word)(WordSet)(Symbol)(Node)
    (NodeSet)(NodeSetSet)(Edge)(EdgeSet)(Delim.Excl_no_bs)
  include A
end


(* This is not as quick (Q) but allows you to specify delimiters *)  
module Make_DQ
  (Vocab:Symbols.SYMBOL_SEED) 
  (Node:X.X_TYPE) 
  (WordD:Delim.DELIM_TYPE) 
  (WordSetD:Delim.DELIM_TYPE) 
  (EdgeD:Delim.DELIM_TYPE) 
  (NodeSetD:Delim.DELIM_TYPE) 
  (NodeSetSetD:Delim.DELIM_TYPE) 
  (EdgeSetD:Delim.DELIM_TYPE)
  (D:Delim.DELIM_TYPE) =

struct
  module Word = Xlist.Make(WordD)(Vocab);;
  module WordSet = Xset.Make(WordSetD)(Word)
  module Symbol = Symbols.Make(Vocab);;
  module NodeSet = Xset.Make(NodeSetD)(Node)
  module NodeSetSet = Xset.Make(NodeSetSetD)(NodeSet)
  module Edge = Edges.Make(Node)(Symbol)(EdgeD)
  module EdgeSet = Xset.Make(EdgeSetD)(Edge)
  module A = Make(Vocab)(Word)(WordSet)(Symbol)(Node)
    (NodeSet)(NodeSetSet)(Edge)(EdgeSet)(D)
  include A
end
