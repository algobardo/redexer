(* * Copyright (c) 2010-2014, * Gianluca Mezzetti <mezzetti@cs.au.dk * All *)
(* rights reserved. * * Redistribution and use in source and binary forms, *)
(* with or without * modification, are permitted provided that the         *)
(* following conditions are met: * * 1. Redistributions of source code     *)
(* must retain the above copyright notice, * this list of conditions and   *)
(* the following disclaimer. * * 2. Redistributions in binary form must    *)
(* reproduce the above copyright notice, * this list of conditions and the *)
(* following disclaimer in the documentation * and/or other materials      *)
(* provided with the distribution. * * 3. The names of the contributors    *)
(* may not be used to endorse or promote * products derived from this      *)
(* software without specific prior written * permission. * * THIS SOFTWARE *)
(* IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" * AND ANY *)
(* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE *     *)
(* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR      *)
(* PURPOSE * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR      *)
(* CONTRIBUTORS BE * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, *)
(* EXEMPLARY, OR * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,   *)
(* PROCUREMENT OF * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR    *)
(* PROFITS; OR BUSINESS * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY   *)
(* OF LIABILITY, WHETHER IN * CONTRACT, STRICT LIABILITY, OR TORT          *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) * ARISING IN ANY WAY OUT OF THE USE *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE * POSSIBILITY OF SUCH DAMAGE.  *)
(***********************************************************************)
(* Data Flow Analysis                                                             *)
(***********************************************************************)

module UiActions =
struct
		
type uiaction =
	| Click
	| Assert
	| InsertText
	| Timeout
	| Nterm of string


end

module ActionsString =
struct
	let name = "ActionString"
  type t = string
  let compare = compare
  let pair x y = y
  let of_string x = x
  let to_string x = x
  let are_compatible x y = x=y
  let conform x y = y

let print ?oc:(oc=stdout) x = 
  output_string oc (to_string x);
  output_string oc "\n";
  flush oc
    
let print_ ?oc:(oc=stdout) x = 
  output_string oc (to_string x);
  flush oc
end



module Word = Xlist.Make(Delim.None)(Vocab);;
module WordSet = Xset.Make(Delim.Newline_no_bs)(Word)
module Symbol = Symbols.Make(Vocab);;
module NodeSet = Xset.Make(Delim.Comma_no_bs)(Node)
module NodeSetSet = Xset.Make(Delim.Newline_no_bs)(NodeSet)
module Edge = Edges.Make(Node)(Symbol)(Delim.Comma_no_bs)
module EdgeSet = Xset.Make(Delim.Newline_no_bs)(Edge)

(* Now we can make the FSA module. *)
module FSA = Acceptors.Make(Vocab)(Word)(WordSet)(Symbol)
  (Node)(NodeSet)(NodeSetSet)(Edge)(EdgeSet)(Delim.Excl_no_bs);;

let join a1 a2 = 
	FSA.minimize (FSA.union a1 a2)

let changed a1 a2 =
	FSA.are_isomorphic (FSA.minimize a1) (FSA.minimize a2) 

let rec rmedge edgl =
	match edgl with
	| [] -> []
	| hd::tl -> (Edge.origin) hd :: (rmedge tl)  

let withnoout a = 
	NodeSet.diff (FSA.nodes a) (NodeSet.of_list (rmedge (EdgeSet.elements (FSA.edges a))))
	


let widening a1 =
	(* attach final nodes without outgoing edges to the start node*)
	let finals = FSA.finals a1
	and nout = withnoout a1 in
	let endpoints = NodeSet.inter finals nout in
	let actaut = ref a1 in
	NodeSet.iter (fun start ->
  	(
			NodeSet.iter (fun x ->
  		let edge = Edge.make x start (Symbol.of_string "")  in
  		let newedges = EdgeSet.add edge (FSA.edges a1) in
			actaut := (FSA.make (FSA.starts !actaut) (FSA.finals !actaut)  newedges) 
  		) endpoints
		) 
	)
	(FSA.starts a1)

let seq_fsa a1 a2 = 
	let newedges = ref EdgeSet.empty in
	NodeSet.iter (
		fun dest ->
		NodeSet.iter (fun src ->
			let edge = Edge.make src dest (Symbol.of_string "") in
			newedges := EdgeSet.add edge !newedges
		)
		(FSA.finals a1)
	)
	(FSA.starts a2);
	FSA.make (FSA.starts a1) (FSA.finals a2) (EdgeSet.union (EdgeSet.union (FSA.edges a1) (FSA.edges a1)) !newedges)

type nd_kind =
  | Standard of content
  | MethodEntry of string
  | MethodExit of string
and content =
  { 
		instr : Dex.link;
		in_abs_state : absState;
		out_abs_state : absState
  }
and absState =
{
	mutable actions : FSA.t
}

module V = struct
  type t = nd_kind
  let compare = Pervasives.compare
  let hash v = 
		match v with 
		| Standard (cnt) -> (Hashtbl.hash cnt.instr)
		| _ -> Hashtbl.hash v
  let equal = (=)
end
module DFG = Graph.Imperative.Digraph.ConcreteBidirectional(V)


type data_flow_graph =
{
  mutable graph : DFG.t;
}

(* ------------------ DEX utils -----------------------------------*)

let find_called_ins (dx : Dex.dex) (ins : Dex.link) : Dex.link list =
  if not (Dex.is_ins dx ins)
  then []
  else
    (let (op, opr) = Dex.get_ins dx ins
     in
     match Instr.access_link op with
     | Instr.METHOD_IDS -> (* last opr at invoke-kind must be method id *)
       let itf = Dex.opr2idx (Util.get_last opr) in
       let iid = Dex.get_cid_from_mid dx itf in
       let cname = Dex.get_ty_str dx iid

       and mname = Dex.get_mtd_name dx itf

       and impls = Dex.get_implementers dx iid
       in
       (
				
       	Log.v ("a call for " ^ (cname ^ ("-" ^ (mname ^ (" with " ^  ((string_of_int (List.length impls)) ^ " implementers"))))));
         let per_impl acc cid =
           (
						try
              let (callee, _) = Dex.get_the_mtd dx cid mname
              in callee :: acc
            with 
						| Dex.Wrong_dex _ -> acc
						| Not_found -> acc
					)
         in 
				List.fold_left per_impl [itf] impls
       )
     | _ -> [])


(* ------------------ Node utils -----------------------------------*)

let start_method dfg methodname =
  MethodEntry methodname

let end_method dfg methodname =
  MethodExit methodname


(** -------------- DFG creation ----------------------- *)


let make_dfg_insrs instr dfg =
	let instradd it dfg =
  	Standard { 
			instr = it;
			in_abs_state = {actions = FSA.empty};
			out_abs_state = {actions = FSA.empty}
		} in

	let inst_nd = List.map (fun it -> instradd it dfg) instr in
	let (first,prev) = (ref None,ref None) in
	List.iter (
		fun it -> 
			match !first with
			| None -> 
				DFG.add_vertex (dfg.graph) it;
				first := Some(it);
				prev := Some(it)
			| Some(_) ->
				let Some(p) = !prev in
				DFG.add_vertex (dfg.graph) it;
				DFG.add_edge (dfg.graph) p it;
				prev := Some(it)
	) inst_nd;
	let (Some(first),Some(last)) = (!first,!prev) in
	(first,last)
	
let map_to_dfg_index all_links cfg_link =
  let matching_nd =
    List.find
      (fun link ->
         match link with
         | (blk_start, blk_last, cfg_index) ->
           cfg_index == cfg_link)
      all_links
  in
  match matching_nd with
  | (blk_start, blk_last, cfg_index) -> blk_start

let find_instr dfg in_link =
	let ind = ref None in
	 DFG.iter_vertex (fun v ->
		match v with 
		| Standard (cont) -> if cont.instr = in_link then ind := Some v else ()
		| _ -> ()
		) dfg.graph;
	match !ind with
	| None -> raise Not_found
	| Some v -> v

let add_nodes methodname cfg dfg = 
	(* links contains a tuple: in_dfg, out_dfg, ind_cfg *)
	(* matching the node ind_cfg in the cfg with two index in the dfg *)
	(* The index in_dfg gets all the incoming edges directed to ind_cfg *)
	(* The index out_dfg gets all the outcoming edges from ind_cfg *)
   let links = DynArray.make 0
   in
   (* Add all instruction blocks and build mapping *)
	let links = ref [] in
	let find_byin y = List.find (fun x -> match x with (a,b,c) -> a = y) !links in
	let find_byout y = List.find (fun x -> match x with (a,b,c) -> b = y) !links in
	let find_bycfg y = List.find (fun x -> match x with (a,b,c) -> c = y) !links in
	
		Array.iteri   
			(fun cfg_index cfg_item ->
				match cfg_item.Ctrlflow.kind with 
				| Ctrlflow.STRT -> 
					let mstart_node = (start_method dfg methodname) in  
					DFG.add_vertex (dfg.graph) mstart_node;
					links := (mstart_node,mstart_node,cfg_item)::!links
				| Ctrlflow.END ->  
					let mend_node = (end_method dfg methodname) in
					DFG.add_vertex (dfg.graph) mend_node;
					links := (mend_node,mend_node,cfg_item)::!links
				| Ctrlflow.NORM ->
         	let (blk_start, blk_last) = make_dfg_insrs cfg_item.Ctrlflow.insns dfg in
					links := (blk_start, blk_last,cfg_item)::!links
			)
      cfg;
			
		List.iter 	
			(fun x ->
				match x with 
				| (ins,outs,cfgi) ->
					List.iter 
					(
						fun succcfg -> 
							let (destin,_,_) = find_bycfg (Array.get cfg succcfg) in
							DFG.add_edge (dfg.graph) outs destin;
					) cfgi.Ctrlflow.succ
			)
		!links	

let convert_cfg methodname cfg =
		let dfg = {graph = DFG.create ()} in
    add_nodes methodname cfg dfg;
		dfg
		
		
		(* --------------- Dotify ------------------*)
			
let ins_toDOT_str addr (op, opr) =
  let s1 =
    Printf.sprintf "0x%08X: %s" (Dex.of_off addr) (Instr.op_to_string op) in
  let s2 = List.fold_left (fun s opr -> s ^ (Instr.opr_to_string opr)) "" opr
  in s1 ^ s2

let node_to_str nd i = "bb" ^ (string_of_int i)

let nodelabel dx nd =
	let i = (Hashtbl.hash nd) in
  match nd with
  | MethodEntry name ->
    Printf.sprintf "  bb%d [label=\"MethodEntry %s -- %d\"];\n" i
      (Netencoding.Url.encode name) i
  | MethodExit name ->
    Printf.sprintf "  bb%d [label=\"MethodExit %s -- %d\"];\n" i
      (Netencoding.Url.encode name) i
  | (Standard cnt) ->
    let s1 = Printf.sprintf "  bb%d [label=\"" i in
    let s2 =
      (ins_toDOT_str cnt.instr (Dex.get_ins dx cnt.instr)) ^
      (Printf.sprintf "\\l") in
    let s3 = Printf.sprintf " -- %d\"];\n" i
    in (* \\l : left-justified vs. \\n : centered *) s1 ^ (s2 ^ s3)

let dot_prologue_str (g_name : string) =
  (Printf.sprintf "digraph %s {\n" g_name) ^
  " node [shape=record fontname=\"courier\"]\n"

let edge_toDOT_str g s d =
  let i = string_of_int  (Hashtbl.hash s)
  and  j = string_of_int (Hashtbl.hash d)
  in Printf.sprintf "  bb%s -> bb%s\n" i j

(* cfg2dot : D.dex -> cfg -> string *)
let gendfg2dot (dx : Dex.dex) g nd_print : string =
  let pre = dot_prologue_str "cfg" in
  let nodes = Buffer.create 10000
  in
		DFG.iter_vertex
     (fun it -> Buffer.add_string nodes (nd_print dx it)) g.graph;
  let edges = Buffer.create 10000 in
    
	DFG.iter_edges
       (fun s d -> Buffer.add_string edges (edge_toDOT_str g s d )) g.graph;
  pre ^ ((Buffer.contents nodes) ^ ((Buffer.contents edges) ^ "}\n"))
	
	
let dfg2dot (dx : Dex.dex) g : string =		
	gendfg2dot (dx : Dex.dex) g nodelabel

(* ----------------- Graphmlfy ---------------------------- *)

(*

let ins_toDOT_str addr (op, opr) =
  let s1 =
    Printf.sprintf "0x%08X: %s" (Dex.of_off addr) (Instr.op_to_string op) in
  let s2 = List.fold_left (fun s opr -> s ^ (Instr.opr_to_string opr)) "" opr
  in s1 ^ s2

let node_str dx nd =
  match nd with
  | MethodEntry name ->
    Printf.sprintf  "MethodEntry: %s"
      (Netencoding.Url.encode name)
  | MethodExit name ->
    Printf.sprintf "MethodEntry: %s"
      (Netencoding.Url.encode name)
  | Standard cnt ->
    let s =
      (ins_toDOT_str cnt.instr (Dex.get_ins dx cnt.instr)) in
    s


  module Gr = struct
    include DFG
  	
  	let vertex_properties = ["id","string",None]
  	let edge_properties = []
  	let map_vertex = fun v dx -> ["d4", node_str dx v ] 
  	let map_edge = fun e dx -> [] 
  	let vertex_uid = DFG.V.hash
  	let edge_uid e = Hashtbl.hash (vertex_uid (DFG.E.src e), DFG.E.label e, vertex_uid (DFG.E.dst e))
  end

 module GraphPrinter = Mygraphml.Print (DFG)(Gr)
*)
(* ---------------------------- Data Flow Analysis --------*)

let nametoaction nm =
	if BatString.exists nm "Solo.click" then
		FSA.of_string "C"
	else
		FSA.empty


let processnd node dfa dx =
	match node with 
	| MethodEntry (_) -> 
		DFG.succ (dfa.graph) node
	| MethodExit (_) -> 
		DFG.succ (dfa.graph) node
	| Standard (cnt) ->
		(* Joining all previous out in current in *)
		let preds = List.filter (fun x -> match x with Standard _ -> true | _ -> false) (DFG.pred dfa.graph node) in
		let preds_out = List.map (fun x -> match x with Standard cnt -> cnt.out_abs_state.actions) preds in
		cnt.in_abs_state.actions <- List.fold_left (fun res it -> join res it) cnt.in_abs_state.actions preds_out;
		
		let dexcalled = find_called_ins dx cnt.instr in
  	let namecalled = List.map (
				fun n ->
					Dex.get_mtd_full_name dx n
			) dexcalled in
		let actioncalled = List.map nametoaction namecalled in 
		let newactions = List.fold_left (fun st nm -> FSA.union nm st) FSA.empty actioncalled in
		let newout = FSA.union cnt.in_abs_state.actions (seq_fsa cnt.in_abs_state.actions newactions) in 
		if changed newout cnt.out_abs_state.actions then
		(
			cnt.out_abs_state.actions <- newout;
			DFG.succ (dfa.graph) node
		)
		else
			[]
 		
let rec run_work_list list dfa dx = 
	  match list with 
		| [] -> ()
		| hd::tl -> 
			let add = processnd hd  dfa dx in 
			run_work_list (tl @ add) dfa dx


(* ---------------------------- Main methods -------------- *)

let findmentry dfg = 
	let found = ref None in
	DFG.iter_vertex (fun v -> 
		match v with
		| MethodEntry _ -> found := Some(v)
		| _ -> ()
		) dfg.graph;
	match !found with 
	| Some (x) -> x
	| _ -> Log.w "findmentry does not find any entry"; raise Not_found

let savefile filename content =
  let oc = open_out ("dot/" ^ ((Netencoding.Url.encode filename) ^ ".dot"))
  in (Printf.fprintf oc "%s" content; close_out oc)

let merge_cfg_into_dfgs (dx : Dex.dex) =
  let process_method i (mtd : Dex.method_id_item) =
    let mid = Dex.to_idx i in
    let method_class_id = Dex.get_cid_from_mid dx mid
    in
    try
      let (_, method_code_item) = Dex.get_citm dx method_class_id mid
      in
      (Log.v
         ("Preparing control flow graph for method " ^
          (Dex.get_mtd_full_name dx mid));
       let method_cfg = Ctrlflow.make_cfg dx method_code_item
       in
       (* ( Log.v ("Control flow graph ready for method "^            *)
       (* (Dex.get_mtd_full_name dx mid)); );                         *)
       method_cfg)
    with
    | Dex.Wrong_dex s ->
      (Log.v
         ("Not found code item for " ^ (Dex.get_mtd_full_name dx mid));
       Ctrlflow.
         make_empty_cfg)
    | _ -> (Log.v "Unmanaged exception"; Ctrlflow.make_empty_cfg)
  in
  (
	Log.v "Getting cfg for all methods";
   let methods_ids = dx.d_method_ids in
   let all_cfgs = DynArray.to_list (DynArray.mapi process_method methods_ids) in
  Log.v "All cfgs made";
	 let to_dfg i (cfg : Ctrlflow.cfg) =
     let methodname = Dex.get_mtd_full_name dx (Dex.to_idx i) in 
     convert_cfg methodname cfg
   in
		 let alldfgs = BatList.mapi to_dfg all_cfgs in
		List.combine (DynArray.to_list (DynArray.mapi (fun i it -> Dex.get_mtd_full_name dx (Dex.to_idx i)) methods_ids)) alldfgs
	)

(* modify *)
let make_dfa (dx : Dex.dex) : unit =
  (
		Log.v "Action graph requested"; 
		let alldfgs = merge_cfg_into_dfgs dx in
		let nonempty = List.filter (fun it -> let (name,x) = it in not (DFG.is_empty x.graph) ) alldfgs in
		List.iter (fun it ->
			let (mname,dfg) = it in
			let mentry = (findmentry dfg) in
				Log.v ("Working on " ^ mname);
				run_work_list [mentry] dfg dx
		) nonempty; 
		(*Print all dfg*)
		(*let nonemptydfg = List.filter (fun it -> let (name,x) = it in BatString.exists name "CreateTrack" ) alldfgs in*)
		(*List.iter (fun it -> let (mname,dfg) = it in savefile mname (dfg2dot dx dfg)) nonemptydfg*)
		(*List.iter (fun it -> let (mname,dfg) = it in savefile mname (GraphPrinter.print dx dfg.graph)) nonemptydfg*)
	)
	
	
	
	
	
	