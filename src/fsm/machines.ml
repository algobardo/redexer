module type MACHINE_TYPE =
sig
  include X.X_TYPE
  type nodeSet
  type edgeSet
  val empty : t
  val make : nodeSet -> nodeSet -> edgeSet -> t
  val of_file : string -> t
  val to_file : string -> t -> unit
  val to_dotstring : ?size:string -> ?rd:string -> ?shape:string -> 
    ?fs:string -> string -> t -> string
  val to_dotfile : ?size:string -> ?rd:string -> ?shape:string -> 
    ?fs:string -> string -> string -> t -> unit

  val report : t -> unit

  (* properties of machines *)
  val starts : t -> nodeSet
  val finals : t -> nodeSet
  val edges : t -> edgeSet
  val nodes : t -> nodeSet
  val is_cyclic : t -> bool
  val is_stripped : t -> bool

  (* comparing machines *)
  val are_isomorphic : t -> t -> bool
  val equal : t -> t -> bool

  (* unary operations *)
  val reverse : t -> t
  val prefix : t -> t  
  val suffix : t -> t
  val trim : t -> t
  val rename : t -> t
  val rename_n : int -> t -> t

  (* binary operations *)
  val inter : t -> t -> t
  val union : t -> t -> t
end


module Make 
  (Node:X.X_TYPE)
  (NodeSet:Xset.XSET_TYPE with type elt = Node.t) 
  (Edge:Edges.EDGE_TYPE with type node = Node.t) 
  (EdgeSet:Xset.XSET_TYPE with type elt = Edge.t) 
  (D:Delim.DELIM_TYPE) =
	  
struct
  (* BASICS *)

  let name = "FSM"

  type node = Node.t
  type nodeSet = NodeSet.t
  type edge = Edge.t
  type edgeSet = EdgeSet.t
  type label = Edge.label

  type t = {starts:nodeSet;finals:nodeSet;edges:edgeSet}
  let compare = compare
  let make s f e = {starts=s;finals=f;edges=e}
  let empty = make
    (NodeSet.singleton (Node.of_string "0"))
    NodeSet.empty
    EdgeSet.empty

  let starts m = m.starts
  let finals m = m.finals
  let edges m = m.edges
  let nodes m =  NodeSet.union
    (EdgeSet.fold 
      (fun e ns -> NodeSet.add (Edge.terminus e) 
	(NodeSet.add (Edge.origin e) ns)
      )
      (edges m)
      NodeSet.empty
    )
    (NodeSet.union (starts m) (finals m))


  let equal m1 m2 = 
    NodeSet.equal (starts m1) (starts m2) 
    && NodeSet.equal (finals m1) (finals m2)
    && EdgeSet.equal (edges m1) (edges m2)

  (* STRINGS AND PRINTING AND FILES *)

  let to_string m = 
    D.lb^
    (NodeSet.to_string (starts m))^D.delim^"\n"^
      (EdgeSet.to_string (edges m))^D.delim^"\n"^
      (NodeSet.to_string (finals m))^D.rb


  let of_string string =
    (* s looks like: starts!edges!finals *)
    let s = (fst (Mstring.extractBetweenDelimiters string D.lb D.rb 0)) in
    let sl = Mstring.to_stringlist D.delim s in
    if List.length sl <> 3
    then raise (Failure("Machine.of_string:Too many or two few arguments "))
    else
      let starts = try NodeSet.of_string (List.nth sl 0)
	with _ -> raise (Failure("Machine.of_string:NodeSet starts"))
      in
      let edges = try EdgeSet.of_string (List.nth sl 1)
	with | (Failure(x)) -> raise (Failure(x))
	  | _ -> raise (Failure("Machine.of_string:EdgeSet edges"))
      in
      let finals = try NodeSet.of_string (List.nth sl 2)
	with _ -> raise (Failure("Machine.of_string:NodeSet finals"))
      in
      make starts finals edges

  let to_file filename m = File.of_string filename (to_string m)
  let of_file filename = of_string (File.to_string ~delim:"\n" filename)

  let print ?oc:(oc=stdout) m = 
    output_string oc (to_string m);
    output_string oc "\n";
    flush oc
      
  let print_ ?oc:(oc=stdout) m = 
    output_string oc D.lb;
    NodeSet.print_ ~oc (starts m);
    output_string oc D.delim;
    EdgeSet.print_ ~oc (edges m);
    output_string oc D.delim;
    NodeSet.print_ ~oc (finals m);
    output_string oc D.rb;
    flush oc

(* GRAPHVIZ *)
      
  let preamble size rd shape fs graphname =  
    "digraph "^graphname^" {\n\n"^
      "graph [size=\""^size^"\", rankdir="^rd^"];\n"^
      "node [shape="^shape^", peripheries=1, fontsize="^fs^"];\n"^
      "edge [minlen=2, fontsize="^fs^"];\n\n" 

  let startShape = "[shape=hexagon]"	
  let finalShape = "[peripheries=2]"
  let dotline = "\n"

  let to_dotstring
    ?size:(size="6,6") ?rd:(rd="LR") ?shape:(shape="circle") ?fs:(fs="18")
    graphname fsa = 
    (preamble size rd shape fs graphname)^dotline^
      (EdgeSet.fold (fun e s -> (Edge.to_dotstring e)^s)
	(edges fsa) dotline)^dotline^
      (NodeSet.fold (fun n s -> "\""^(Node.to_string n)^"\" "^
	startShape^dotline^s)
	(starts fsa) dotline)^
      (NodeSet.fold (fun n s -> "\""^(Node.to_string n)^"\" "^
	finalShape^dotline^s)
	(finals fsa) dotline)^
      "}" 

  let to_dotfile
    ?size:(size="6,6") ?rd:(rd="LR") ?shape:(shape="circle") ?fs:(fs="18")
    path graphname fsa = 
    File.of_string (path^graphname^".dot") 
      (to_dotstring ~size:"6,6" ~rd:"LR"
	~shape:"circle" ~fs:"18" graphname fsa)

  let report t = 
    let s = NodeSet.cardinal (starts t) in
    let f = NodeSet.cardinal (finals t) in
    let n = NodeSet.cardinal (nodes t) in
    let e = EdgeSet.cardinal (edges t) in
    print_endline ("start states: "^(string_of_int s));
    print_endline ("final states: "^(string_of_int f));
    print_endline ("total states: "^(string_of_int n));
    print_endline ("transitions: "^(string_of_int e))

  (* TOOLS *)

  type direction = Forward | Backward 

  let forward = Forward
  let backward = Backward

  let directed_origin d e = 
    match d with 
      |Forward -> Edge.origin e
      |_ -> Edge.terminus e

  let directed_terminus d e = 
    match d with 
      |Forward -> Edge.terminus e
      |_ -> Edge.origin e

  let origin = Edge.origin
  let terminus = Edge.terminus 

  type edge_point = Origin | Terminus

  let ep_to_node ep e =
    match ep with | Origin -> origin e | Terminus -> terminus e 

  let edgeset_assign_node ep edgeset node = 
    EdgeSet.fold
      (fun e es -> 
	let newedge = match ep with
	  | Origin -> Edge.make node (Edge.terminus e) (Edge.label e)
	  | Terminus -> Edge.make (Edge.origin e) node (Edge.label e)
	in
	EdgeSet.add newedge es) edgeset EdgeSet.empty


  (* REVERSE *)
  let reverse_edges edgeset = EdgeSet.fold
    (fun e es -> 
      EdgeSet.add 
	(Edge.make (Edge.terminus e) (Edge.origin e) (Edge.label e))
	es
    )
    edgeset
    EdgeSet.empty
    
  let reverse m = make (finals m) (starts m) (reverse_edges (edges m))

  let prefix m = make (starts m) (nodes m) (edges m)
  let suffix m = make (nodes m) (finals m) (edges m)

  (* TRIM *)
  (* A trimmed acceptor has no useless states. A states is useless if it is not
     accessible from a start state or cannot lead to a final state
     (i.e. accessible from a final state when run in reverse). *) 

  let rec accessibleHelper currentNodes accessibleNodes remainingEdges d = 
    let foldFunction edge (nodeSet,edgeSet) =  
      let practicalTerminus = directed_terminus d edge in 
      let fromCurrentNodes = 
	(NodeSet.mem (directed_origin d edge) currentNodes)
      in  
      let alreadyVisited = NodeSet.mem practicalTerminus accessibleNodes 
      in 
      match (fromCurrentNodes,alreadyVisited) with   
	| (true,false) -> (NodeSet.add practicalTerminus nodeSet,edgeSet)
	| (_,false) -> (nodeSet,(EdgeSet.add edge edgeSet))
	| _ -> (nodeSet,edgeSet)
    in  
    let (newNodes, newEdges) = 
      EdgeSet.fold foldFunction remainingEdges (NodeSet.empty,EdgeSet.empty)  
    in   
    if (NodeSet.is_empty newNodes) 
    then accessibleNodes 
    else 
      accessibleHelper 
	newNodes
	(NodeSet.union newNodes accessibleNodes)
	newEdges
	d

  let get_accessible_nodes direction nodeSet edgeSet = 
    accessibleHelper nodeSet nodeSet edgeSet direction
      
  let useful_nodes m = 
    NodeSet.inter 
      (get_accessible_nodes forward (starts m) (edges m)) 
      (get_accessible_nodes backward (finals m) (edges m)) 

  let useful_edge nodeSet edge = 
    (NodeSet.mem (origin edge) nodeSet) &&
    (NodeSet.mem (terminus edge) nodeSet) 

  let trim m = 
    let useful_nodeset = useful_nodes m in 
    make 
      (NodeSet.inter (starts m) useful_nodeset)
      (NodeSet.inter (finals m) useful_nodeset)
      (EdgeSet.filter (fun x -> useful_edge useful_nodeset x) (edges m))


  (* IS STRIPPED *)
  let is_stripped m = 
    NodeSet.is_empty (NodeSet.diff (nodes m) (useful_nodes m))


  (* IS CYCLIC *)
  (* this function finds the set of nodes accessible from a set of nodes
     excluding the nodeset given *)
  let get_accessible_nodes_excl direction nodeSet edgeSet = 
    accessibleHelper nodeSet NodeSet.empty edgeSet direction
  
  let is_cyclic m =
    let edgesM = edges m in
    let nodesM = nodes m in
    let rec helper n rns =                (* rns: remaining node set *)
      if NodeSet.mem n 
	(get_accessible_nodes_excl forward (NodeSet.singleton n) edgesM)
      then true
      else let next_rns = NodeSet.remove n rns in
	   if NodeSet.is_empty next_rns then false 
	   else helper (NodeSet.choose next_rns) next_rns
    in 
    helper (NodeSet.choose nodesM) nodesM


  (* RENAME STATES *)

  let node_of_int x = Node.of_string (string_of_int x)

  (* change_edges returns a pair of edgesets, the altered versions of inputs
     es1 and es2. It finds edges in es1 whose origin (or terminus) match node
     and then change that origin (or terminus) to newNode. Whether the origin
     or terminus is targeted is set according to f.
  *)

  let change_edges ep es1 es2 node newNode =
      let to_change_edges = EdgeSet.filter (fun x -> ep_to_node ep x = node) es1 in 
      let changed_edges = edgeset_assign_node ep to_change_edges newNode in
      (EdgeSet.union changed_edges es2), (EdgeSet.diff es1 to_change_edges)

  (* ues : Edges in this Set are Unchanged
     cos : edges in this Set have Changed Origins (but not termini)
     cts : edges in this Set have Changed Termini (but not origins)
     cbs : edges in this Set have Changed Both origins and termini *)
	
  let update_cbs cos cts cbs node newNode =
    let cbs2,cts2 = change_edges Origin cts cbs node newNode in
    let cbs3,cos2 = change_edges Terminus cos cbs2 node newNode in
    cbs3,cos2,cts2

  let update_edges ues cos cts cbs node newNode =
    let new_cos,ues2 = change_edges Origin ues cos node newNode in
    let new_cts,ues3 = change_edges Terminus ues2 cts node newNode in
    let cbs2,cos2,cts2 = update_cbs new_cos new_cts cbs node newNode in
    ues3,cos2,cts2,cbs2

  let update_nodeset newNS node newNode oldNS=
    if NodeSet.mem node oldNS 
    then NodeSet.add newNode newNS
    else newNS

  let update_nodelist node nodelist finished_nodeset edgesM =
    let edges_from_origin = 
      EdgeSet.filter (fun x -> (origin x) = node) edgesM
    in 
    let label_sort e1 e2 =
      Edge.label_compare (Edge.label e1) (Edge.label e2)
    in
    let edgeList = 
      List.sort	label_sort (EdgeSet.elements edges_from_origin)
    in 
    let terminiList = List.map (fun x -> terminus x) edgeList
    in 
    List.fold_left (fun a terminus -> 
      if NodeSet.mem terminus finished_nodeset || List.mem terminus a 
      then a else terminus::a)
      nodelist
      terminiList

  (* if two machines have the same edges, but nodes with different names, then
     this function should retuern the same machine when applied to each of the
     two machines. *)
      
  let rename_n num m = 
    let startsM = starts m in
    let finalsM = finals m in
    let edgesM = edges m in
    (* fns: finished node set *)
    let rec helper nodelist fns ues cos cts cbs finals starts n =
      if nodelist = []
      then (starts,finals,cbs)
      else 
	let node = List.hd nodelist in
	let newNode = node_of_int n in
	let (ues2,cos2,cts2,cbs2) = 
	  update_edges ues cos cts cbs node newNode
	in 
	let finals2 = update_nodeset finals node newNode finalsM in
	let starts2 = update_nodeset starts node newNode startsM in
	let fns2 = NodeSet.add node fns in
	let nodelist2 = update_nodelist node (List.tl nodelist) fns2 edgesM 
	in 
	helper nodelist2 fns2 ues2 cos2 cts2 cbs2 finals2 starts2 (n+1)
    in 
    let newStarts,newFinals,newEdges = 
      helper (NodeSet.elements startsM) NodeSet.empty edgesM EdgeSet.empty 
	EdgeSet.empty EdgeSet.empty NodeSet.empty NodeSet.empty num
    in 
    make newStarts newFinals newEdges;;
  
  let rename m = rename_n 0 m;;

  (* INTER *)
  let inter m1 m2 = make 
    (NodeSet.cross (starts m1) (starts m2))
    (NodeSet.cross (finals m1) (finals m2))
    (EdgeSet.cross (edges m1) (edges m2))

  (* UNION *) 
  let is_node_name_conflict m1 m2 = 
    let nodes1 = (nodes m1) and nodes2 = (nodes m2) in
    NodeSet.fold 
      (fun n bool -> bool ||
	if NodeSet.mem n nodes2 then true
	else false)
      nodes1 false

  let union m1 m2 = 
    let rm1,rm2 = 
      if is_node_name_conflict m1 m2 
      then rename m1, let num = NodeSet.cardinal (nodes m1) in
			     rename_n num m2
      else m1,m2
    in make
    (NodeSet.union (starts rm1) (starts rm2))
    (NodeSet.union (finals rm1) (finals rm2))
    (EdgeSet.union (edges rm1) (edges rm2))

  let pair = inter


  (* ARE ISOMORPHIC *)

  (* I sneak in label sets using Edge.label_compare *)
  module OrderedLabels  =
  struct type t = Edge.label let compare = Edge.label_compare  end
    
  module LabelSet = Set.Make(OrderedLabels)


  (* Generally, we use OrderedNodePairs to keep track of equivalent
     nodes. *)
    
  module OrderedNodePairs = 
  struct     
    type t = Node.t * Node.t 
    let compare (a,b) (c,d) = 
      let r = Node.compare a c in
      if r = 0 then Node.compare b d else r
  end


  module NxNSet = Set.Make(OrderedNodePairs)

  module NxNSetSet = Set.Make(NxNSet)

  (* the idea implemented here is that each label is associated with a set of
     node pairs in a machine -- the (origin,terminus) pairs for which there is
     an edge in the machine bearing that label. For each machine then each label
     set is associated with a set of NxN sets. If the sets of NxN sets are the
     same then the edges in the machines are isomorphic. *)

  let edgeset_to_labelset es = 
    EdgeSet.fold (fun e ls -> LabelSet.add (Edge.label e) ls) es LabelSet.empty
    
  let label_to_NxNSet l m = 
    EdgeSet.fold (fun e nxnset -> 
      if (Edge.label e) = l then 
	NxNSet.add (origin e, terminus e) nxnset
      else nxnset)
      (edges m) NxNSet.empty
      
  let make_nxnss m =
    LabelSet.fold 
      (fun l nxnss -> NxNSetSet.add (label_to_NxNSet l m) nxnss)
      (edgeset_to_labelset (edges m))
      NxNSetSet.empty

  let are_isomorphic m1 m2 = 
    let rm1 = rename m1 in
    let rm2 = rename m2 in
    let nxnss1 = make_nxnss rm1 in
    let nxnss2 = make_nxnss rm2 in
    NodeSet.equal (starts rm1) (starts rm2)
    && NodeSet.equal (finals rm1) (finals rm2)
    && NxNSetSet.equal nxnss1 nxnss2

end
