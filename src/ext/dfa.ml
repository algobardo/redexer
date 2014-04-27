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


type nd_kind =
  | Standard of content
  | MethodEntry of string
  | MethodExit of string
and content =
  { instr : Dex.link; abs_state : absState
  }
and absState =
  | Nothing

module V = struct
  type t = nd_kind
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end
module DFG = Graph.Imperative.Digraph.ConcreteBidirectional(V)



type data_flow_graph =
{
  graph : DFG.t;
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
  	Standard { instr = it; abs_state = Nothing } in

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
	Log.v ("First " ^ (string_of_int (Hashtbl.hash first)));
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
	| None -> Log.v "Index not found ";raise Not_found
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
	let findin y = List.find (fun x -> match x with (a,b,c) -> a = y) !links in
	let findout y = List.find (fun x -> match x with (a,b,c) -> b = y) !links in
	let findcfg y = List.find (fun x -> match x with (a,b,c) -> c = y) !links in
	
		Array.iteri   
			(fun cfg_index cfg_item ->
				match cfg_item.Ctrlflow.kind with 
				| Ctrlflow.STRT -> 
					let mstart_node = (start_method dfg methodname) in  
					DFG.add_vertex (dfg.graph) mstart_node;
					List.iter (fun i -> links := (mstart_node,mstart_node,i)::!links) cfg_item.Ctrlflow.succ
				| Ctrlflow.END ->  
					let mend_node = (end_method dfg methodname) in
					DFG.add_vertex (dfg.graph) mend_node;
					List.iter (fun i -> links := (mend_node,mend_node,i)::!links) cfg_item.Ctrlflow.succ
				| Ctrlflow.NORM ->
         	let (blk_start, blk_last) = make_dfg_insrs cfg_item.Ctrlflow.insns dfg in
					List.iter (fun i -> links := (blk_start, blk_last,i)::!links) cfg_item.Ctrlflow.succ
			)
      cfg;
			
		List.iter 	
			(fun x ->
				()
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

let bb2node_str dx nd =
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
let dfg2dot_str (dx : Dex.dex) g : string =
  let pre = dot_prologue_str "cfg" in
  let nodes = Buffer.create 10000
  in
		DFG.iter_vertex
     (fun it -> Buffer.add_string nodes (bb2node_str dx it)) g.graph;
  let edges = Buffer.create 10000 in
    
	DFG.iter_edges
       (fun s d -> Buffer.add_string edges (edge_toDOT_str g s d )) g.graph;
  pre ^ ((Buffer.contents nodes) ^ ((Buffer.contents edges) ^ "}\n"))
	
	
		


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

(*let add_method_links dfg links =
	
	(* Methods for removing invocation links *)
	let remove_single_link id = 
		let nd = get_byind dfg id in
		let nextinstruction = DynArray.get (get_structure nd).next 0 in
		(*Log.v "instruction after " ^ (str_of_instr_node nd) ^ (str_of_DfgInd nextinstruction)*)
		DynArray.clear (get_structure nd).next;
		(id,nextinstruction)
	in
	let rec remove_links ls =
		match ls with
		| hd::tl -> remove_single_link hd :: (remove_links tl)
		| [] -> []
	in
	(* Disconecting method invocation *)
	let all_invocations = List.map (fun x -> match x with (src,dst) -> src ) links in
	let removed_links = remove_links all_invocations in
	
	List.iter
	(
	fun link ->
		match link with 
		| (src,dest) ->
			let srcnd = get_byind dfg src in
			match srcnd with
			| (src_str,_) ->
				(* Add the MethodEntry node as next of method invocation*)
				DynArray.add src_str.next dest;
				(* Add to the MethodExit node the instruction following the invocation*)
				let (exit_str,_) = get_mexit_byentry dfg dest in
				let (_,invocation_nextinstr) =  List.find (fun x -> let (s,d) = x in s=src) removed_links in
				DynArray.add exit_str.next invocation_nextinstr
	)			
	links
*)
(*
let forward_analysis dx (dfg: data_flow_graph)= 
	
	let get_invoked_links i nd =
		(
		match nd with
		| (_,Standard (cont)) ->
			let called = find_called_ins dx cont.instr in
			let mappedcalled = List.fold_left (fun ls cld_link -> 
				try (
					(map_methodlink2dfgindex dx dfg cld_link)::ls
					)
					with 
					| Not_found -> ls
					) [] called in
			(List.map (fun cld_link -> (DfgInd(i), cld_link)) mappedcalled) 
		| _ -> []
		)
	in
	let all_links_list = DynArray.mapi get_invoked_links (dfg.graph) in
	let all_links = DynArray.fold_left (fun ls it -> List.append ls it) [] all_links_list in
	add_method_links dfg all_links
	*)
	
(* ---------------------------- Main methods -------------- *)


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
		let nonemptydfg = List.filter (fun it -> let (name,x) = it in BatString.exists name "CreateTrack" ) alldfgs in
		List.iter (fun it -> let (mname,dfg) = it in savefile mname (dfg2dot_str dx dfg)) nonemptydfg
		(*List.iter (fun it -> let (mname,dfg) = it in savefile mname (GraphPrinter.print dx dfg.graph)) nonemptydfg*)
	)
	
	
	
	
	
	