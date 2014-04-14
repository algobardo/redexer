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

class tester (dx: Dex.dex) =
	let strs = Modify.new_ty dx (Java.to_java_ty ("["^Java.Lang.str)) in
	let [logger_cid] = List.map (Dex.get_cid dx) ["Solo"] in
	(* let log_mid, _ = Dex.get_the_mtd dx logger_cid "act" in *)
	object
		inherit Visitor.iterator dx
		
		val mutable do_this_cls = false
		val mutable cur_cls = ""
		val mutable cur_scls = ""
		
		method v_cdef (cdef: Dex.class_def_item) : unit =
			let cid = cdef.Dex.c_class_id
			and sid = cdef.Dex.superclass in
			cur_cls <- Java.of_java_ty (Dex.get_ty_str dx cid);
			cur_scls <- Java.of_java_ty (Dex.get_ty_str dx sid);
			do_this_cls <- (BatString.exists cur_scls "android.test.ActivityInstrumentationTestCase2");
			if do_this_cls then
				Log.i ("Working on Test Case: " ^ cur_cls);
		
		method v_emtd (emtd: Dex.encoded_method) : unit =
			if do_this_cls then
				let mname = Dex.get_mtd_name dx emtd.Dex.method_idx in
				Log.i ("Working on single test:" ^ mname );
		
		method v_citm (citm: Dex.code_item) : unit =
			if do_this_cls then
				Log.i ("citm");
		
		method v_ins (ins: Dex.link) : unit =
			if do_this_cls then
				Log.i ("vins");
		
		method finish () : unit =
			Log.i ("Finish ");
		
	end

class soloTimeoutRemover (dx: Dex.dex) timeout =
	let rec stupidnop x = 
		match x with
		| 0 -> []
		| _ -> Instr.make_instr Instr.OP_NOP []::stupidnop (x-1)
	in
object
	inherit Visitor.iterator dx
	
	(* Whether are we instrumenting the current visited class *)
	val mutable do_this_cls = false
	(* Whether are we instrumenting the current visited method *)
	val mutable do_this_method = false
	(* timeout arg location the method signature *)
	val mutable argi = -1
	
	method v_cdef (cdef: Dex.class_def_item) : unit =
		let cid = cdef.Dex.c_class_id
		and sid = cdef.Dex.superclass in
		let cur_cls = Java.of_java_ty (Dex.get_ty_str dx cid) in
		do_this_cls <- (BatString.ends_with cur_cls "com.robotium.solo.Solo");
		if do_this_cls then
			Log.i ("Working on Solo class: " ^ cur_cls);
	
	method v_emtd (emtd: Dex.encoded_method) : unit =
		if do_this_cls then
			let mname = Dex.get_mtd_name dx emtd.Dex.method_idx in
			do_this_method <- (BatString.exists mname "waitFor");
			if do_this_method then
				(
					(* Getting the method arguments *)
					let argvs = Dex.get_argv dx (Dex.get_mit dx (emtd.Dex.method_idx)) in
					(* Finding the one with type long *)
					try
						let (i, _) = BatList.findi
								(fun i arg ->
											BatString.exists (Dex.get_ty_str dx arg) (Java.to_type_descr "long"))
								argvs
						in
						argi <- i;
						Log.i ("Working on :" ^ Dex.get_mtd_sig dx emtd.Dex.method_idx ^ ", the long is arg number " ^ string_of_int i )
					with
					| Not_found -> do_this_method <- false
				)
	
	method v_citm (citm: Dex.code_item) : unit =
		if (do_this_cls && do_this_method) then
			(
				(* We add an instruction setting the timeout to a desired value *)
				let ins0 = Instr.new_const ((Dex.calc_this citm) + argi + 1) timeout in
				(Modify.insrt_insns_before_start dx citm [ins0]);
				(*(Modify.insrt_insns_after_start dx citm (stupidnop 50));
				(Modify.insrt_insns_before_start dx citm (stupidnop 50));*)
				Log.i "Instruction inserted"
			)
	
	method finish () : unit =
		Log.i ("Finish ");
	
end

(* modify *)
let modify (dx: Dex.dex) : unit =
	Log.set_level "debug";
	(* Visitor.iter (new tester dx); *)
	Visitor.iter (new soloTimeoutRemover dx 10);
	Modify.expand_opr dx;
	Modify.seed_addr dx.Dex.header.Dex.file_size;
	Log.i "Starting dumping";
  (* Dump the rewritten dex *)
	(Dump.dump "tests_apk/rewrite.dex") dx 


