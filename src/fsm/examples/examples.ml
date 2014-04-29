(* Example Usage *)

(* Make sure the tools.cma is loaded *)

#load "FSA.cma"

(* Here we create modules which will serve as input to the Acceptors.Make
   functor.
 *)
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

(* Here is a simple acceptor a*ba*. The delimiters that are used can be
   customized. *)
let astarbastar = FSA.of_string 
"0!
0,0,a
0,1,b
1,1,a
!1";;

FSA.accepts astarbastar "aaaabaaaaaaa";; 

let ws = FSA.generate astarbastar 10;;
WordSet.print ws;;

(* Alternatively we can read a FSA from a file *)
let m = FSA.of_file "pintupi.fsa";;
FSA.is_cyclic m;;

(* Here we make a prefix tree from a word list.*)
let m = FSA.make_pt "wordlist";;

(* Now we can convert the FSA to a dot file *)
FSA.to_dotfile "." "prefix_tree" m;;

(* Now we convert that to a gif file. Open it for viewing. *)
File.dot2x "prefix_tree.dot" "gif";;

(* Lets make the node shapes be ellipses. *)
FSA.to_dotfile ~shape:"ellipse" "." "prefix_tree2" mm;;
File.dot2x "prefix_tree2.dot" "gif";;


(* Now we build a prefix tree from the language where words have to have an
   even number of zeros and an even number of ones *)
let zr0 = FSA.make_pt "examples/zr_words" ;;
FSA.to_dotfile "examples/" "zr_pt" zr0;
File.dot2x "examples/zr_pt.dot" "gif";;

(* Now we merge states to make a zer0-reversible language *)
let zr = FSA.rename (FSA.merge zr0 FSA.b_successors);;
FSA.to_dotfile "examples/" "zr_lg" zr;;
File.dot2x "examples/zr_lg.dot" "gif";;
