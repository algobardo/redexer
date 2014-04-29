

(** 
    This is documentation for the command line executable "fsa".

    author: Jeff Heinz
 
    last updated: July 30, 2006

*)

(**

   This page explains how to use the [fsa] command line executable.  Usage of
   this function allows one to apply a variety of operations and functions to
   finite state acceptors (FSAs).  With this command it is straightforward to take
   unions and intersections of FSAs, to determinize, minimize, reverse FSAs,
   to rename the states of an FSA and to query certain properties of FSAs.  It
   is also straightforward to generate words from a FSA, build prefix trees
   from a finite set of words etc. 


*)

(** {3 Usage} *)

(** {2 Invocation} *)

(** Like other OCaML executables, the program is run by typing [./fsa] or
    [ocamlrun fsa] at the command prompt.  If you compiled using [ocamlopt]
    then you can just type [fsa]. Generally it is invoked as follows.
    
    [fsa command] {i flags} {i file}
    
*)

(** {2 Input and Output} *)

(**
   The output of the [fsa] command is always written to standard output.
   Therefore it is possible to pipe it [|] to other commands or direct it [>]
   to a file.  The finite state acceptor that the [fsa] command operates on
   is typically identified by a filename. However, if no filename is given,
   the fsa can be entered from standard input. Use [^D] to send the input to
   the command.

*)

(** {2 Commands} *)


(**   By itself [fsa] will not do anything-- the argument [command] is mandatory.
   The [command] arguments recognized by [fsa] are shown in the list below.
   The command is illustrated without any flag arguments assuming that
   [file] contains an FSA. A brief description is also given.
   - "to_dot" : [fsa to_dot file] converts the FSA in [file] to a form that can be interpreted by AT&T's
     {{:http://graphviz.org/}Graphviz} drawing program.
   - "accepts" : [fsa accepts word file] outputs "true" iff the FSA in [file] accepts [word].
   - "generate" : [fsa generate N M file] outputs all words between sizes [N] and [M]
     accepted by the FSA in [file].
   - "merge" : [fsa merge eqr file] outputs an FSA that is the result of merging states
      in blocks induced by the equivalence relation [eqr]. See below.
   - "make_pt" : [fsa make_pt file] outputs a prefix tree for the words in [file].
   - "make_pt2" : [fsa make_pt2 file] outputs a prefix tree for the words in [file]. 
      This is faster and can handle files much larger than [make_pt]
   - "make_st2" : [fsa make_st2 file] outputs a suffix tree for the words in [file].
      This is faster and can handle files much larger than [make_st]
   - "concat" : [fsa concat file1 file2] outputs an acceptor which accepts 
      the concatenation of the language accepted by the fsa in [file1] and 
      the language accepted by the fsa in [file2].
   - "inter" : [fsa inter file1 file2] outputs the intersection of the FSAs in [file1]
      and [file2].
   - "union" : [fsa union file1 file2] outputs the union of the FSAs in [file1] and [file2].
   - "star" : [fsa star file] outputs an fsa which accepts the star closure of the language 
      accepted by the fsa in [file].
   - "reverse" : [fsa reverse file] outputs the reverse of the FSA in [file]
   - "complement" : [fsa complement file] outputs the complement of the FSA in [file].
   - "cmp_determinize" : [fsa cmp_determinize] completely determinizes the complement of the FSA in [file].
   - "determinize" : [fsa determinize file] outputs the trimmed result of [cmp_determinize]
      applied to the FSA in [file].
   - "rename" : [fsa rename file] outputs the same FSA except the states have been
      renamed with numbers, beginning with 0. If a number follows [rename] then the acceptor 
      begin renaming with that number. E.g. [fsa rename 3 file] renames the fsa in [file] 
      beginning with 3.
   - "minimize" : [fsa minimize file] outputs an FSA isomorphic to the canonical acceptor
      for the FSA in [file].
   - "trim" : [fsa trim file] outputs an FSA with no useless states otherwise identical
      to the fsa in [file].
   - "equal" : [fsa equal file1 file2] outputs "true" iff the FSA in [file1] is exactly
      identical to the FSA in [file2].
   - "isomorphic" : [fsa isomorphic file1 file2] outputs "true" iff the FSA in [file1] is
      isomorphic to the FSA in [file2].
   - "is_cyclic" : [fsa is_cyclic file] outputs "true" iff the FSA in [file1] is cyclic.
   - "is_stripped" : [fsa is_stripped file] outputs "true" iff the FSA in [file1] has no
      useless states.
   - "is_det" : [fsa is_det file]  outputs "true" iff the FSA in [file1] is
        deterministic.
   - "report" : [fsa report file] outputs to standard output the number of start states, 
      final states, and the total number of states and transitions of the fsa in [file].
   - "format" : [fsa format formatfile fsafile] prints the fsa in [fsafile] according to 
      the formatting specifications in [formatfile].
*)


(** {2 Equivalence relations for [merge]} *)

(**
     The equivalence relations that are implemented are: 
     - "nhoods" [fsa merge nhoods j k file] merges states with the same j-k neighborhoods.
     - "leaders" [fsa merge leaders k file] merges states with the k-leaders 
     (See {i Learning Locally Testable Languages in the Strict Sense} (Garcia et. al 1990)).
     - "followers" [fsa merge followers k file] merges states with the k-followers.
     - "is_final" [fsa merge is_final file] merges final states. 
     (See {i Learning Left-to-Right and Right-to-Left Iterative Languages} (Heinz 2008)).
     - "is_start" [fsa merge is_start file] merges start states.
     - "is_nonfinal" [fsa merge is_nonfinal file] merges nonfinal states.
     - "is_nonstart" [fsa merge is_nonstart file] merges nonstart states.
     - "ZR" [fsa merge ZR] recursively merges states which share b-successors.
     (See {i Inference of Reversible Languages} (Angluin 1982)).

 *)

(*
     - "ngrams" : [fsa merge ngrams N file] merges states which have the same N-0
     neighborhoods.  The user should make sure the FSA in [file] is
     deterministic if the user want to be guaranteed to get a N-gram language.  
*)


(** {3 Flags} *)

(**     
	Finally, in the event you want to appy more than one function to an FSA
	there are flags available that allow you can use. Of course you can
	always pipe the command to another [fsa] command, but a flag requires
	less typing. These are discussed below.
*)


(** Following the first argument (the main command) you can optionally list as many of the following
    flags as you like (separated by spaces):
    - "-rn" : renames the output of the the main command beginning with 0.
    - "-m"  : minmizes the output of the the main command
    - "-rv"  : reverses the output of the the main command
    - "-dt"  : determinizes the output of the the main command
    - "-cdt"  : completely determinizes the output of the the main command
    - "-c"  : complements the output of the the main command
    - "-t"  : trims the output of the the main command

    When these are stacked they are applied in left to right order. For
    example, the command [fsa union -rv -m -rn fsa1 fsa1] does the following:

    [rename(minimize(reverse(union fsa1 fsa2)))]


*)


(** {3 Reading and Writing FSAs} *)


(** 

    You can secify how you want to write the FSAs by customizing the
    "fsa_specs" file. When you run [fsa] it reads the finite state acceptor
    from the input according to the specifications in this file in the current
    directory. (If no file is in the directory, it uses default values.)

    A FSA consists of three components:
    - the start states
    - the transitions
    - the final states

    These must be seperated by the FSA delimiter (default "!").  The start
    states should also be delimited by something (default ","). Similarly, the
    transitions should be delimited by something (default "\n"; i.e a newline).a
    The final states are delimited in the same way as the start states.

    A transition also consists of three components:
    - the origin state
    - the terminus state
    - the label

    These components should also be separated by a delimiter (default ",").
    If desired, it is also possible to specify left brace and right brace
    strings for the various components mentioned above.

    As an example, the following is a well-formed acceptor.  

    {v 
    A!

    A,B,2
    B,C,0
    C,E,1
    E,C,0
    C,D,0
    
    !B,C,D    v}

    See the file "examples/Pintupi.fsa" for more info.
*)

(** {3 Writing DOT files and Drawing} *)

(** When using the command "to_dot", [fsa] reads the "dot_specs" file in the
    current directory to see
    how you want to the machine to appear. If there is no file named "dot_specs"
    in the current directory, [fsa] uses default values. There are only a few customizable
    options. They are:
    - "size" : The dimensions of the graph (default "6,6").
    - "rd" : Rank Direction. Possibilities are "LR" (left-to-right) or "TD"
    (top-to-down). The default is "LR".
    - "shape" : The shape of the states. The default is "circle". If the state
    names are long, "ellipse" is often a good choice.
    - "fs" : Font size of the labels and state names (default "18").

    Note that the {{:http://graphviz.org/}Graphviz} drawing program allows
    further customization.  For the most part the options above provide enough
    flexibility for readable graphs. If you want further customization just
    produce and then edit the dot file directly.
*)


