(* fsa.ml contains code that runs from the command line *)



let num_args = (Array.length Sys.argv)-1 ;;
(* let () = print_endline ("n:"^(string_of_int num_args))*)
(* let () = print_endline ("0:"^(Sys.argv.(0))) *)
(* let () = print_endline ("1:"^(Sys.argv.(1))) *)
(* let () = print_endline ("2:"^(Sys.argv.(2))) *)
(* let () = print_endline ("3:"^(Sys.argv.(3)))*)

let arg_list = List.tl (Array.to_list Sys.argv)
let error = ref false;;


(* establish the formatting of the fsas *)


let default_dot_specs = [":setting";"size:6,6";
			 "rd:LR";"shape:circle";"fs:18"]

let default_fsa_specs = [":lb:rb:delim";"Word:::";
			 "WordS:::\n";"Edge:::,";"NodeS:::,";
			 "NodeSS:::\n";"EdgeS:::\n";"FSA:::!"]

let t_dot = 
  try Table.of_file "dot_specs" 
  with (Sys_error(_)) -> 
    Table.of_stringlist ~col_delim:':' default_dot_specs

let t_fsa = 
  try
    let spec = List.hd arg_list in
      if spec = "-spec" then
	try let specfile = List.hd (List.tl arg_list) in
	      Table.of_file specfile 
	with _ -> failwith "Insufficient arguments: -spec requires one argument."
      else raise Not_found
  with _ -> 	
    try Table.of_file "fsa_specs" 
    with (Sys_error(_)) -> 
      Table.of_stringlist ~col_delim:':' default_fsa_specs

let rec scan arglist = 
  match arglist with
    | [] -> raise Not_found
    | h::t -> if h = "format" 
      then try List.hd t 
	with _ -> failwith "Insufficient arguments: [format] requires one argument."
      else scan t

let t_fsa2 = 
  try let specfile = scan arg_list in
	Table.of_file specfile 
  with Not_found -> 
    try Table.of_file "fsa_specs"
    with (Sys_error(_)) -> 
      Table.of_stringlist ~col_delim:':' default_fsa_specs

let process s = match s with 
  | "<nl>" -> "\n" 
  | "<sp>" -> " "
  | _ -> s

let drf = "Row name not understood in dot specs"
let dcf = "Column name not understood in dot specs"
let frf = "Row name not understood in fsa specs"
let fcf = "Column name not understood in fsa specs"

(* Getting the delimiters right for fsa *)

let dot_size = process (Table.get_val t_dot "size" "setting" drf dcf)
let dot_rd = process (Table.get_val t_dot "rd" "setting" drf dcf)
let dot_shape = process (Table.get_val t_dot "shape" "setting" drf dcf)
let dot_fs = process (Table.get_val t_dot "fs" "setting" drf dcf)

let word_lb = process (Table.get_val t_fsa "Word" "lb" frf fcf)
let word_rb = process (Table.get_val t_fsa "Word" "rb" frf fcf)
let word_delim = process (Table.get_val t_fsa "Word" "delim" frf fcf)
let wordS_lb = process (Table.get_val t_fsa "WordS" "lb" frf fcf)
let wordS_rb = process (Table.get_val t_fsa "WordS" "rb" frf fcf)
let wordS_delim = process (Table.get_val t_fsa "WordS" "delim" frf fcf)
let edge_lb = process (Table.get_val t_fsa "Edge" "lb" frf fcf)
let edge_rb = process (Table.get_val t_fsa "Edge" "rb" frf fcf)
let edge_delim = process (Table.get_val t_fsa "Edge" "delim" frf fcf)
let nodeS_lb = process (Table.get_val t_fsa "NodeS" "lb" frf fcf)
let nodeS_rb = process (Table.get_val t_fsa "NodeS" "rb" frf fcf)
let nodeS_delim = process (Table.get_val t_fsa "NodeS" "delim" frf fcf)
let nodeSS_lb = process (Table.get_val t_fsa "NodeSS" "lb" frf fcf)
let nodeSS_rb = process (Table.get_val t_fsa "NodeSS" "rb" frf fcf)
let nodeSS_delim = process (Table.get_val t_fsa "NodeSS" "delim" frf fcf)
let edgeS_lb = process (Table.get_val t_fsa "EdgeS" "lb" frf fcf)
let edgeS_rb = process (Table.get_val t_fsa "EdgeS" "rb" frf fcf)
let edgeS_delim = process (Table.get_val t_fsa "EdgeS" "delim" frf fcf)
let fsa_lb = process (Table.get_val t_fsa "FSA" "lb" frf fcf)
let fsa_rb = process (Table.get_val t_fsa "FSA" "rb" frf fcf)
let fsa_delim = process (Table.get_val t_fsa "FSA" "delim" frf fcf)

module Word_D   = struct  let lb = word_lb   let rb = word_rb   let delim = word_delim   end
module WordS_D  = struct  let lb = wordS_lb  let rb = wordS_rb  let delim = wordS_delim  end
module Edge_D   = struct  let lb = edge_lb   let rb = edge_rb   let delim = edge_delim   end
module NodeS_D  = struct  let lb = nodeS_lb  let rb = nodeS_rb  let delim = nodeS_delim  end
module NodeSS_D = struct  let lb = nodeSS_lb let rb = nodeSS_rb let delim = nodeSS_delim end
module EdgeS_D  = struct  let lb = edgeS_lb  let rb = edgeS_rb  let delim = edgeS_delim  end
module FSA_D    = struct  let lb = fsa_lb    let rb = fsa_rb    let delim = fsa_delim    end

module Word = Xlist.Make(Word_D)(Vocab)
module WordSet = Xset.Make(WordS_D)(Word)
module Symbol = Symbols.Make(Vocab)
module NodeSet = Xset.Make(NodeS_D)(Node)
module NodeSetSet = Xset.Make(NodeSS_D)(NodeSet)
module Edge = Edges.Make(Node)(Symbol)(Edge_D)
module EdgeSet = Xset.Make(EdgeS_D)(Edge)

(* Now we can make the FSA module. *)
module FSA = Acceptors.Make(Vocab)(Word)(WordSet)(Symbol)
  (Node)(NodeSet)(NodeSetSet)(Edge)(EdgeSet)(FSA_D);;

(* module FSA = Acceptors.Make_DQ *)
(*   (Vocab) *)
(*   (Node) *)
(*   (Word_D) *)
(*   (WordS_D) *)
(*   (Edge_D) *)
(*   (NodeS_D) *)
(*   (NodeSS_D) *)
(*   (EdgeS_D) *)
(*   (FSA_D) *)

(* Getting the delimiters right for fsa2 *)

let word2_lb = process (Table.get_val t_fsa2 "Word" "lb" frf fcf)
let word2_rb = process (Table.get_val t_fsa2 "Word" "rb" frf fcf)
let word2_delim = process (Table.get_val t_fsa2 "Word" "delim" frf fcf)
let wordS2_lb = process (Table.get_val t_fsa2 "WordS" "lb" frf fcf)
let wordS2_rb = process (Table.get_val t_fsa2 "WordS" "rb" frf fcf)
let wordS2_delim = process (Table.get_val t_fsa2 "WordS" "delim" frf fcf)
let edge2_lb = process (Table.get_val t_fsa2 "Edge" "lb" frf fcf)
let edge2_rb = process (Table.get_val t_fsa2 "Edge" "rb" frf fcf)
let edge2_delim = process (Table.get_val t_fsa2 "Edge" "delim" frf fcf)
let nodeS2_lb = process (Table.get_val t_fsa2 "NodeS" "lb" frf fcf)
let nodeS2_rb = process (Table.get_val t_fsa2 "NodeS" "rb" frf fcf)
let nodeS2_delim = process (Table.get_val t_fsa2 "NodeS" "delim" frf fcf)
let nodeSS2_lb = process (Table.get_val t_fsa2 "NodeSS" "lb" frf fcf)
let nodeSS2_rb = process (Table.get_val t_fsa2 "NodeSS" "rb" frf fcf)
let nodeSS2_delim = process (Table.get_val t_fsa2 "NodeSS" "delim" frf fcf)
let edgeS2_lb = process (Table.get_val t_fsa2 "EdgeS" "lb" frf fcf)
let edgeS2_rb = process (Table.get_val t_fsa2 "EdgeS" "rb" frf fcf)
let edgeS2_delim = process (Table.get_val t_fsa2 "EdgeS" "delim" frf fcf)
let fsa2_lb = process (Table.get_val t_fsa2 "FSA" "lb" frf fcf)
let fsa2_rb = process (Table.get_val t_fsa2 "FSA" "rb" frf fcf)
let fsa2_delim = process (Table.get_val t_fsa2 "FSA" "delim" frf fcf)

module Word_D2   = struct  let lb = word2_lb   let rb = word2_rb   let delim = word2_delim   end
module WordS_D2  = struct  let lb = wordS2_lb  let rb = wordS2_rb  let delim = wordS2_delim  end
module Edge_D2   = struct  let lb = edge2_lb   let rb = edge2_rb   let delim = edge2_delim   end
module NodeS_D2  = struct  let lb = nodeS2_lb  let rb = nodeS2_rb  let delim = nodeS2_delim  end
module NodeSS_D2 = struct  let lb = nodeSS2_lb let rb = nodeSS2_rb let delim = nodeSS2_delim end
module EdgeS_D2  = struct  let lb = edgeS2_lb  let rb = edgeS2_rb  let delim = edgeS2_delim  end
module FSA_D2    = struct  let lb = fsa2_lb    let rb = fsa2_rb    let delim = fsa2_delim    end

module Word2 = Xlist.Make(Word_D2)(Vocab)
module WordSet2 = Xset.Make(WordS_D2)(Word2)
module Symbol2 = Symbols.Make(Vocab)
module NodeSet2 = Xset.Make(NodeS_D2)(Node)
module NodeSetSet2 = Xset.Make(NodeSS_D2)(NodeSet2)
module Edge2 = Edges.Make(Node)(Symbol)(Edge_D2)
module EdgeSet2 = Xset.Make(EdgeS_D2)(Edge2)

(* Now we can make the FSA2 module. *)
module FSA2 = Acceptors.Make(Vocab)(Word2)(WordSet2)(Symbol)
  (Node)(NodeSet2)(NodeSetSet2)(Edge2)(EdgeSet2)(FSA_D2);;


(* the format command converts a acceptor in fsa1 format and prints it in fsa2 format *)

let es2_of_es es = 
  EdgeSet.fold
    (fun e es2 -> EdgeSet2.add 
      (Edge2.make (Edge.origin e) (Edge.terminus e) (Edge.label e))
      es2
    )
    es EdgeSet2.empty

let ns2_of_ns ns = 
  NodeSet.fold
    (fun n ns2 -> NodeSet2.add n ns2)
    ns NodeSet2.empty

let format fsa = 
  FSA2.make
    (ns2_of_ns (FSA.starts fsa))
    (ns2_of_ns (FSA.finals fsa))
    (es2_of_es (FSA.edges fsa))



(* USAGE *)

let usage () = print_endline ("\nUSAGE:\n"^
  "fsa recognizes a number of commands for manipulating\n"^
  "finite state acceptors. These include:\n\n"^
   "to_dot\t\t"^
   "accepts\t\t"^
   "generate\t"^
   "merge\n"^
 
   "make_pt\t\t"^ 
   "make_pt2\t"^ 
   "make_st\t\t"^ 
   "make_st2\n"^ 

   "concat\t\t"^ 
   "inter\t\t"^ 
   "union\t\t"^ 
   "star\t\n"^ 

   "reverse\t\t"^
   "complement\t"^ 
   "cmp_determinize\t"^ 
   "determinize\n"^ 

   "rename\t\t"^ 
   "minimize\t"^ 
   "trim\t\t"^ 
   "equal\n"^ 

   "isomorphic\t"^ 
   "is_cyclic\t"^ 
   "is_stripped\t"^ 
   "is_det\n"^ 

   "format\t"^ 
   "report\n\n"^

  "These commands are described in detail in the documentation.\n");;


(* "Mandatory arguments are given and optional arguments\n"^
  "are in parantheses. If an argument is optional, you\n"^
  "will have to enter the argument via standard input.\n"^
  "Use ^D to indicate you have reached the end of your\n"^
  "input. Notes follow the ':'.\n\n"^
  "fsa recognizes the following commands:\n\n"^
  "2dot (filename)        : produces a string representation\n"^
  "                         interpretable by AT&T's dot program.\n"^
  "reverse (filename)     : reverses an acceptor\n"^
  "complement (filename)  : returns the complement of the\n"^
  "                        acceptor\n"^ 
  "determinize (filename) : determinizes an acceptor\n"^
  "rename (filename)      : renames an acceptor\n"^
  "minimize (filename)    : minimizes an acceptor\n"^
  "trim (filename)        : removes useless states from an \n"^
  "                         acceptor \n\n"^
  "inter (filename1) (filename2) : returns the intersection \n"^
  "union (filename1) (filename2) : returns the union \n"^
  "accepts string (filename)     : returns true iff the fsa in\n"^
  "                                filename accepts string";;

*)

(* the program really begins here *)


let cmd_list = 
  let rec templist arglist = 
    match arglist with
      | [] -> []
      | h::[] -> h::(templist [])
      | h::(a::b) -> 
	  if h = "-spec" then templist b
	  else if h = "format" then h::(templist b)
	  else h::(templist (a::b))
  in
  templist arg_list;;

(* List.iter (fun x -> print_string (x^" ")) cmd_list;; *)
      

let rec action_1 f cmd_list = 
  match cmd_list with
    | [] -> FSA.print (f (FSA.of_string (File.read ())))
    | arg::t -> 
	if arg.[0] = '-'   (* if arg is flag *)
	then match arg with
	  | "-rn" -> action_1 (fun x -> FSA.rename (f x)) t
	  | "-m" -> action_1 (fun x -> FSA.minimize (f x)) t
	  | "-rv" -> action_1 (fun x -> FSA.reverse (f x)) t
	  | "-dt" -> action_1 (fun x -> FSA.determinize (f x)) t
	  | "-cdt" -> action_1 (fun x -> FSA.complete_determinize (f x)) t
	  | "-c" -> action_1 (fun x -> FSA.complement (f x)) t
	  | "-t" -> action_1 (fun x -> FSA.trim (f x)) t
	  | _ -> failwith "Unrecognized flag. See documentation."
	else if t = []
	then FSA.print (f (FSA.of_file arg))
	else failwith "Unrecognized argument. See documentation.";;

let rec action_2 f cmd_list = 
  match cmd_list with
    | [] ->  FSA.print (f (FSA.of_string (File.read ())) (FSA.of_string (File.read ())))
    | arg::t -> if arg.[0] = '-'   (* if arg is flag *)
      then match arg with
	  | "-rn" -> action_2 (fun x y -> FSA.rename (f x y)) t
	  | "-m" -> action_2 (fun x y -> FSA.minimize (f x y)) t
	  | "-rv" -> action_2 (fun x y -> FSA.reverse (f x y)) t
	  | "-dt" -> action_2 (fun x y -> FSA.determinize (f x y)) t
	  | "-cdt" -> action_2 (fun x y -> FSA.complete_determinize (f x y)) t
	  | "-c" -> action_2 (fun x y -> FSA.complement (f x y)) t
	  | "-t" -> action_2 (fun x y -> FSA.trim (f x y)) t
	  | _ -> failwith "Unrecognized flag. See documentation."
      else 
	  match t with 
	    | [] -> FSA.print (f (FSA.of_string (File.read ())) (FSA.of_file arg))
	    | x::[] -> if (List.tl t)=[] then FSA.print (f (FSA.of_file arg) (FSA.of_file x))
	    | _ -> failwith "Unrecognized argument. See documentation.";;


let rec pt_action f pt_type cmd_list =
  match cmd_list with
    | [] -> let ws = (FSA.wordSet_of_string (File.read ())) in
      let tree = match pt_type with
	| "make_pt" -> FSA.pt ws 
	| "make_st" -> FSA.st ws
	| "make_range_fsa" -> FSA.range_fsa ws
	| _ -> failwith "fsa: pt_action: This should be impossible."
      in FSA.print (f tree)

    | arg::t -> if arg.[0] = '-'   (* if arg is flag *)
      then match arg with
	  | "-rn" -> pt_action (fun x -> FSA.rename (f x)) pt_type t
	  | "-m" -> pt_action (fun x -> FSA.minimize (f x)) pt_type t
	  | "-rv" -> pt_action (fun x -> FSA.reverse (f x)) pt_type t
	  | "-dt" -> pt_action (fun x -> FSA.determinize (f x)) pt_type t
	  | "-cdt" -> pt_action (fun x -> FSA.complete_determinize (f x)) pt_type t
	  | "-c" -> pt_action (fun x -> FSA.complement (f x)) pt_type t
	  | "-t" -> pt_action (fun x -> FSA.trim (f x)) pt_type t
	  | _ -> failwith "Unrecognized flag. See documentation."

      else if t = []
      then let tree = match pt_type with
	  | "make_pt" -> FSA.make_pt arg
	  | "make_st" -> FSA.make_st arg
	  | "make_rdfsa" -> FSA.make_range_fsa arg
	  | _ -> failwith "fsa: pt_action: This should be impossible."
	in FSA.print (f tree)
      else failwith "Unrecognized argument. See documentation.";;


let bool_1 f cmd_list = 
  let b = match cmd_list with
    | [] -> f (FSA.of_string (File.read ()))
    | arg::[] -> f (FSA.of_file arg)
    | _ -> failwith "Unrecognized argument. See documentation."
  in
  print_endline (string_of_bool b);;


let bool_2 f cmd_list = 
  let b = 
  match cmd_list with
    | [] -> f (FSA.of_string (File.read ())) (FSA.of_string (File.read ()))
    | arg::[] -> f (FSA.of_string (File.read ())) (FSA.of_file arg)
    | file1::(file2::[]) -> f (FSA.of_file file1) (FSA.of_file file2)
    | _ -> failwith "Unrecognized argument. See documentation."
  in 
  print_endline (string_of_bool b);;


let main () =
  if cmd_list = []
  then usage ()
  else
    let cmd = List.hd cmd_list in
    match cmd with	
      | "to_dot" -> 
	  let s = if (List.tl cmd_list) = []
	    then FSA.to_dotstring 
	      ~size:dot_size ~rd:dot_rd ~shape:dot_shape ~fs:dot_fs 
	      "fsa" (FSA.of_string (File.read ()))
	    else FSA.to_dotstring 
	      ~size:dot_size ~rd:dot_rd ~shape:dot_shape ~fs:dot_fs 
	      "fsa" (FSA.of_file Sys.argv.(2))
	  in
	  print_endline s
	    
    | "accepts" ->
	let b = 
	  match (List.tl cmd_list) with
	    | [] -> failwith "Insufficient number of arguments. See documentation."
	    | w::[] -> FSA.accepts (FSA.of_string (File.read ())) w
	    | w::(f::[]) -> FSA.accepts (FSA.of_file f) w
	    | _ -> failwith "Unrecognized argument. See documentation."
	in
	print_endline (string_of_bool b)
	  
    | "generate" ->  
	begin
	  match (List.tl cmd_list) with
	    | [] -> failwith "Insufficient number of arguments. See documentation."
	    | min::(max::[]) ->  FSA.generate_p (FSA.of_string (File.read ())) (int_of_string min) (int_of_string max)
	    | min::(max::(f::[])) -> FSA.generate_p (FSA.of_file f) (int_of_string min) (int_of_string max)
	    | _ -> failwith "Unrecognized or insufficient arguments. Maybe you didn't include min and max lengths? See documentation."
	end
	      
    | "merge" -> 
	begin
	  match (List.tl cmd_list) with
	    | [] -> failwith "Insufficient number of arguments. See documentation."
	    | eqr::t -> match eqr with
		| "leaders" | "followers" ->
		    begin match t with
		      |  [] -> failwith "Insufficient number of arguments. See documentation."
		      |  ns::t2 -> 
			   let n = int_of_string ns in
			   if eqr = "leaders" then
			     action_1 (fun x -> FSA.merge x (FSA.k_leaders_eqr n)) t2
			   else action_1 (fun x -> FSA.merge x (FSA.k_followers_eqr n)) t2
		    end

		| "nhoods" ->  
		    begin match t with
		      | [] -> failwith "Insufficient number of arguments. See documentation."
		      | n::[] -> failwith "Insufficient number of arguments. See documentation."
		      | js::(ks::t2) -> let j = int_of_string js and k = int_of_string ks in
				      action_1 (fun x -> FSA.merge x (FSA.jk_nhoods_eqr j k)) t2
		    end
		    
		| "ZR" -> action_1 (fun x -> FSA.merge x FSA.b_successors) t
		| "is_final" -> action_1 (fun x -> FSA.merge x FSA.is_final_eqr) t
		| "is_nonfinal" -> action_1 (fun x -> FSA.merge x FSA.is_nonfinal_eqr) t
		| "is_start" -> action_1 (fun x -> FSA.merge x FSA.is_start_eqr) t
		| "is_nonstart" -> action_1 (fun x -> FSA.merge x FSA.is_nonstart_eqr) t
		| _ -> failwith "Unrecognized argument. See documentation."
	end
	
    | "make_pt"
    | "make_st"
    | "make_rdfsa" -> pt_action (fun x -> x) cmd (List.tl cmd_list)

    | "make_pt2" 
    | "make_st2" -> 
	begin
	  match (List.tl cmd_list) with
	    | [] -> failwith "Insufficient number of arguments. See documentation."
	    | t::[] -> let tree = 
		if cmd = "make_pt2" 
		then FSA.make_pt2 t
		else FSA.make_st2 t in 
	      FSA.print tree
	    | _ -> failwith "Unrecognized argument. See documentation."
	end
	
    | "make_fin" -> 	
	begin
	  let ic = 
	    match (List.tl cmd_list) with
	      | [] -> stdin
	      | filename::[] -> open_in filename
	      | _ -> failwith "Unrecognized argument. See documentation."
	  in
	  FSA.print (FSA.make_fin ic)
	end
    | "rename" -> begin 
	  let x = List.hd (List.tl cmd_list) in
	  try let n = int_of_string x in
	      action_1 (FSA.rename_n n) (List.tl (List.tl cmd_list))
	  with (Failure("int_of_string")) -> 
	    action_1 FSA.rename (List.tl cmd_list) end

    | "shuffle_ideal" 
    | "factor_ideal" -> 
	begin
	  let f = if cmd="shuffle_ideal" then FSA.shuffle_ideal else FSA.factor_ideal 
	  in
	  try
	    let alphabetword = Word.of_string (List.hd (List.tl cmd_list)) in
	    let sequence = Word.of_string (List.hd (List.tl (List.tl cmd_list))) in
	      FSA.print (f alphabetword sequence)
          with _ -> failwith "Unrecognized/Incorrect arguments. See documentation."
	end

    | "reverse" -> action_1 FSA.reverse (List.tl cmd_list)
    | "complement" -> action_1 FSA.complement (List.tl cmd_list)
    | "determinize" -> action_1 FSA.determinize (List.tl cmd_list)
    | "cmp_determinize" -> action_1 FSA.complete_determinize (List.tl cmd_list)
    | "minimize" -> action_1 FSA.minimize (List.tl cmd_list)
    | "trim" -> action_1 FSA.trim (List.tl cmd_list)
    | "star" -> action_1 FSA.star (List.tl cmd_list)
    | "prefix" -> action_1 FSA.prefix (List.tl cmd_list)
    | "suffix" -> action_1 FSA.suffix (List.tl cmd_list)
	
    | "inter" -> action_2 FSA.inter (List.tl cmd_list)
    | "union" -> action_2 FSA.union (List.tl cmd_list)
    | "concat" -> action_2 FSA.concat (List.tl cmd_list)
	
    | "equal" -> bool_2 FSA.equal (List.tl cmd_list)
    | "isomorphic" -> bool_2 FSA.are_isomorphic (List.tl cmd_list)

    | "is_cyclic" -> bool_1 FSA.is_cyclic (List.tl cmd_list)
    | "is_stripped" -> bool_1 FSA.is_stripped (List.tl cmd_list)
    | "is_det" -> bool_1 FSA.is_deterministic (List.tl cmd_list)
    | "is_nd" -> begin
	let cmd_tail = List.tl cmd_list in
	match cmd_tail with
	  | jstr::kstr::t -> 
	      let j = int_of_string jstr and
		  k = int_of_string kstr 
	      in bool_1 (FSA.is_nd j k) t
	    | _ -> failwith "Unrecognized argument. See documentation."
      end
    | "report" -> 	begin
	  match (List.tl cmd_list) with
	    | [] -> failwith "Insufficient number of arguments. See documentation."
	    | t::[] -> FSA.report (FSA.of_file t)
	    | _ -> failwith "Unrecognized argument. See documentation."
	end

    | "format" -> begin
	  let cmd_tail = List.tl cmd_list in
	  match cmd_tail with
	    | [] -> FSA2.print (format (FSA.of_string (File.read ())))
	    | h::[] -> FSA2.print (format (FSA.of_file h))
	    | _ -> failwith "Unrecognized argument. See documentation."
      end

    | _ -> usage ();;

main ();;
	

