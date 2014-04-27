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
(* Testing                                                             *)
(***********************************************************************)

type dfg_nodeindex = DfgInd of int

type data_flow_graph =
{
  graph : instr_node DynArray.t;
	mutable mentry : (string -> dfg_nodeindex);
	mutable mexit	: (string -> dfg_nodeindex)
}
and instr_node =
  (gstructure * nd_kind)

and nd_kind =
  | Standard of content
  | MethodEntry of string
  | MethodExit of string
  | BlockEntry
  | BlockExit

and content =
  { instr : Dex.link; abs_state : absState
  }

and absState =
  | Nothing

and gstructure =
  { next : dfg_nodeindex DynArray.t; prev : dfg_nodeindex DynArray.t
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
  let str =
    {
      next = DynArray.of_list [];
      prev = DynArray.of_list [];
    }
  in (str, (MethodEntry methodname))

let end_method dfg methodname =
  let str =
    {
      next = DynArray.of_list [];
      prev = DynArray.of_list [];
    }
  in (str, (MethodExit methodname))

let start_block dfg =
  let str =
    {
      next = DynArray.of_list [ DfgInd( (DynArray.length (dfg.graph)) + 1 ) ];
      prev = DynArray.of_list [];
    }
  in (str, BlockEntry)

let end_block dfg =
  let str =
    {
      next = DynArray.of_list [];
      prev = DynArray.of_list [ DfgInd( (DynArray.length (dfg.graph)) - 1) ];
    }
  in (str, BlockExit)


(* -------------- DFG creation ----------------------- *)

let instradd i it dfg =
  let next = DfgInd( (DynArray.length (dfg.graph)) + 1) in
  let nxarr = DynArray.of_list [ next ] in
  let prevarr = DynArray.of_list [ DfgInd(  (DynArray.length (dfg.graph)) - 1 ) ] in
  let str = { next = nxarr; prev = prevarr; } in
  let knd = Standard { instr = it; abs_state = Nothing; } in (str, knd)

let make_dfg_insrs instr dfg =
  let first = DfgInd( DynArray.length (dfg.graph) )
  in
  (DynArray.add (dfg.graph) (start_block dfg);
   List.iteri (fun i it -> DynArray.add (dfg.graph) (instradd i it dfg)) instr;
   DynArray.add (dfg.graph) (end_block dfg);
   (first , DfgInd((DynArray.length (dfg.graph)) - 1)))

let map_to_dfg_index all_links cfg_link =
  let mathing_link =
    List.find
      (fun link ->
         match link with
         | (blk_start, blk_last, cfg_index) ->
           cfg_index == cfg_link)
      all_links
  in
  match mathing_link with
  | (blk_start, blk_last, cfg_index) -> blk_start



let add_nodes methodname cfg dfg = 
	(* links contains a tuple: in_dfg, out_dfg, ind_cfg *)
	(* matching the node ind_cfg in the cfg with two index in the dfg *)
	(* The index in_dfg gets all the incoming edges directed to ind_cfg *)
	(* The index out_dfg gets all the outcoming edges from ind_cfg *)
   let links = DynArray.make 0
   in
   (* Add all instruction blocks and update links with cfg indices and    *)
   (* dfg indices Perform linking between instruction blocks using links  *)
   Array.iteri
      (fun cfg_index cfg_item ->
				match cfg_item.Ctrlflow.kind with 
				| Ctrlflow.STRT -> 
					let mstart_node = (start_method dfg methodname) in
					let oldmentry = dfg.mentry
					and idx = DynArray.length (dfg.graph) in
				  dfg.mentry <- (fun x -> if (String.compare x methodname) = 0 then DfgInd (idx) else (oldmentry x));  
					DynArray.add (dfg.graph) mstart_node;
					DynArray.add links (DfgInd( DynArray.length (dfg.graph) - 1 ),DfgInd( DynArray.length (dfg.graph) - 1 ), cfg_index)
				| Ctrlflow.END ->  
					let mend_node = (end_method dfg methodname) in
					let oldmexit = dfg.mexit 
					and idx = DynArray.length (dfg.graph) in
					dfg.mexit <- (fun x -> if (String.compare x methodname) = 0 then DfgInd (idx) else oldmexit x);
					DynArray.add (dfg.graph) mend_node;
					DynArray.add links (DfgInd( DynArray.length (dfg.graph) - 1 ), DfgInd( DynArray.length (dfg.graph) - 1 ), cfg_index)
				| Ctrlflow.NORM ->
         let (blk_start, blk_last) =
           make_dfg_insrs cfg_item.Ctrlflow.insns dfg
         in DynArray.add links (blk_start, blk_last, cfg_index))
      cfg;
	DynArray.to_list links

let convert_cfg methodname cfg dfg =
    let linkslist = add_nodes methodname cfg dfg in
     List.iter
       (fun it ->
          match it with
          | (DfgInd(blk_start),DfgInd(blk_last), cfg_index) ->
            let dfglinks =
              List.map (map_to_dfg_index linkslist)
                (Array.get cfg cfg_index).Ctrlflow.succ in
            let (str, _) = DynArray.get (dfg.graph) blk_last
            in DynArray.append (DynArray.of_list dfglinks) str.next)
       linkslist

(* ----------------- Dotify ---------------------------- *)

let ins_toDOT_str addr (op, opr) =
  let s1 =
    Printf.sprintf "0x%08X: %s" (Dex.of_off addr) (Instr.op_to_string op) in
  let s2 = List.fold_left (fun s opr -> s ^ (Instr.opr_to_string opr)) "" opr
  in s1 ^ s2

let node_to_str nd i = "bb" ^ (string_of_int i)

let bb2node_str dx i nd =
  match nd with
  | (_, MethodEntry name) ->
    Printf.sprintf "  bb%d [label=\"MethodEntry %s -- %d\"];\n" i
      (Netencoding.Url.encode name) i
  | (_, MethodExit name) ->
    Printf.sprintf "  bb%d [label=\"MethodExit %s -- %d\"];\n" i
      (Netencoding.Url.encode name) i
  | (_, BlockEntry) ->
    Printf.sprintf "  bb%d [label=\"BlockEntry -- %d\"];\n" i i
  | (_, BlockExit) ->
    Printf.sprintf "  bb%d [label=\"BlockExit -- %d\"];\n" i i
  | (_, Standard cnt) ->
    let s1 = Printf.sprintf "  bb%d [label=\"" i in
    let s2 =
      (ins_toDOT_str cnt.instr (Dex.get_ins dx cnt.instr)) ^
      (Printf.sprintf "\\l") in
    let s3 = Printf.sprintf " -- %d\"];\n" i
    in (* \\l : left-justified vs. \\n : centered *) s1 ^ (s2 ^ s3)

let dot_prologue_str (g_name : string) =
  (Printf.sprintf "digraph %s {\n" g_name) ^
  " node [shape=record fontname=\"courier\"]\n"

let edge_toDOT_str g i j =
	match (i,j) with
	| (DfgInd(indi),DfgInd(indj)) ->
  let i = string_of_int indi
  and  j = string_of_int indj
  in Printf.sprintf "  bb%s -> bb%s\n" i j

(* cfg2dot : D.dex -> cfg -> string *)
let dfg2dot_str (dx : Dex.dex) g : string =
  let pre = dot_prologue_str "cfg" in
  let nodes = Buffer.create 10000
  in
  (
		DynArray.iteri
     (fun i it -> Buffer.add_string nodes (bb2node_str dx i it)) g;
   let dot_edge i nd =
     match nd with
     | (str, cnt) ->
       DynArray.fold_left (fun s it -> s ^ (edge_toDOT_str g i it)) ""
         str.next
   in
   (
    let edges = Buffer.create 10000
    in
    (DynArray.iteri
       (fun i it -> Buffer.add_string edges (dot_edge (DfgInd(i)) it)) g;
     pre ^
     ((Buffer.contents nodes) ^ ((Buffer.contents edges) ^ "}\n"))))
	)

(* ---------------------------- Data Flow Analysis --------*)

let get_instr_link nd =
	match nd with
	| (_,Standard(cont)) -> cont.instr
	| _ -> assert false

let get_structure nd =
	match nd with
	| (str,_) -> str


let get_byind dfg ind =
	match ind with 
	| DfgInd(i) -> Log.v ("Getting index" ^ (string_of_int i) ^ "of" ^ (string_of_int (DynArray.length dfg.graph)));DynArray.get (dfg.graph) i 



let map_methodlink2dfgindex (dx:Dex.dex) (dfg:data_flow_graph) (link:Dex.link) =
	let mname = Dex.get_mtd_full_name dx link in
	dfg.mentry mname
	
let get_mexit_byentry dfg id =
		let (_,MethodEntry(mname)) = get_byind dfg id in
		get_byind dfg (dfg.mexit mname)

let add_method_links dfg links =
	
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
	
	
(* ---------------------------- Main methods -------------- *)


let savefile filename content =
  let oc = open_out ("dot/" ^ (filename ^ ".dot"))
  in (Printf.fprintf oc "%s" content; close_out oc)

let merge_cfg_into_dfg (dx : Dex.dex) =
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
		Log.v "Starting building action graph";
   let methods_ids = dx.d_method_ids in
   let all_cfgs = DynArray.mapi process_method methods_ids in
   let converted_dfg = {graph=DynArray.make 0;mentry=(fun x -> raise Not_found);mexit=(fun x -> raise Not_found)} in
   let mergecfg_indfg i (cfg : Ctrlflow.cfg) =
     let methodname = Dex.get_mtd_full_name dx (Dex.to_idx i)
     in
     if BatString.exists methodname "CreateTrack" 
     then convert_cfg methodname cfg converted_dfg
     else ()
   in
		DynArray.iteri mergecfg_indfg all_cfgs;
		forward_analysis dx converted_dfg;
    let dfgdot = dfg2dot_str dx converted_dfg.graph
    in savefile "allmethods" dfgdot
		)

(* modify *)
let modify (dx : Dex.dex) : unit =
  (
		Log.v "Action graph requested"; 
		merge_cfg_into_dfg dx
	)
	
	
	
	
	
	
	(* OLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLDOLD *)
	
	
	
	
	
type ('termt, 'ntermt) cfg =
  'ntermt -> ((('termt, 'ntermt) term_or_nterm) list) list

and ('termt, 'ntermt) term_or_nterm =
  | Term of 'termt | NTerm of 'ntermt

type ui_action = | Click of string | InsertText of (string * string)

class tester (dx : Dex.dex) =
  let strs = Modify.new_ty dx (Java.to_java_ty ("[" ^ Java.Lang.str))
  in let ([ logger_cid ]) = List.map (Dex.get_cid dx) [ "Solo" ]
  in (* let log_mid, _ = Dex.get_the_mtd dx logger_cid "act" in *)
  object inherit Visitor.iterator dx
    val mutable do_this_cls = false
    val mutable cur_cls = ""
    val mutable cur_scls = ""

    method v_cdef =
      fun (cdef : Dex.class_def_item) ->
        (let cid = cdef.Dex.c_class_id

         and sid = cdef.Dex.superclass
         in
         (cur_cls <- Java.of_java_ty (Dex.get_ty_str dx cid);
          cur_scls <- Java.of_java_ty (Dex.get_ty_str dx sid);
          do_this_cls <-
            BatString.exists cur_scls
              "android.test.ActivityInstrumentationTestCase2";
          if do_this_cls
          then Log.i ("Working on Test Case: " ^ cur_cls)
          else ()) :
           unit)

    method v_emtd =
      fun (emtd : Dex.encoded_method) ->
        (if do_this_cls
         then
           (let mname = Dex.get_mtd_name dx emtd.Dex.method_idx
            in Log.i ("Working on single test:" ^ mname))
         else () : unit)

    method v_citm =
      fun (citm : Dex.code_item) ->
        (if do_this_cls then Log.i "citm" else () : unit)

    method v_ins =
      fun (ins : Dex.link) ->
        (if do_this_cls then Log.i "vins" else () : unit)

    method finish = fun () -> (Log.i "Finish " : unit)
  end

class soloTimeoutRemover (dx : Dex.dex) timeout =
  let rec stupidnop x =
    match x with
    | 0 -> []
    | _ -> (Instr.make_instr Instr.OP_NOP []) :: (stupidnop (x - 1))
  in
  object inherit Visitor.iterator dx

    (* Whether are we instrumenting the current visited class *)
    val mutable do_this_cls = false

    (* Whether are we instrumenting the current visited method *)
    val mutable do_this_method = false

    (* timeout arg location the method signature *)
    val mutable argi = (-1)

    method v_cdef =
      fun (cdef : Dex.class_def_item) ->
        (let cid = cdef.Dex.c_class_id and sid = cdef.Dex.superclass in
         let cur_cls = Java.of_java_ty (Dex.get_ty_str dx cid)
         in
         (do_this_cls <-
            BatString.ends_with cur_cls "com.robotium.solo.Solo";
          if do_this_cls
          then Log.i ("Working on Solo class: " ^ cur_cls)
          else ()) :
           unit)

    method v_emtd =
      fun (emtd : Dex.encoded_method) ->
        (if do_this_cls
         then
           (let mname = Dex.get_mtd_name dx emtd.Dex.method_idx
            in
            (do_this_method <- BatString.exists mname "waitFor";
             if do_this_method
             then (* Getting the method arguments *)
               (let argvs =
                 Dex.get_argv dx (Dex.get_mit dx emtd.Dex.method_idx)
                in
                (* Finding the one with type long *)
                try
                  let (i, _) =
                    BatList.findi
                      (fun i arg ->
                         BatString.exists (Dex.get_ty_str dx arg)
                           (Java.to_type_descr "long"))
                      argvs
                  in
                  (argi <- i;
                   Log.i
                     ("Working on :" ^
                      ((Dex.get_mtd_sig dx emtd.Dex.method_idx) ^
                       (", the long is arg number " ^
                        (string_of_int i)))))
                with | Not_found -> do_this_method <- false)
             else ()))
         else () : unit)

    method v_citm =
      fun (citm : Dex.code_item) ->
        (if do_this_cls && do_this_method
         then
           (* We add an instruction setting the timeout to a desired      *)
           (* value                                                       *)
           (let ins0 =
             Instr.new_const (((Dex.calc_this citm) + argi) + 1) timeout
            in
            (Modify.insrt_insns_before_start dx citm [ ins0 ];
             (* (Modify.insrt_insns_after_start dx citm (stupidnop      *)
             (* 50)); (Modify.insrt_insns_before_start dx citm          *)
             (* (stupidnop 50));                                        *)
             Log.i "Instruction inserted"))
         else () : unit)

    method finish = fun () -> (Log.i "Finish " : unit)
  end
