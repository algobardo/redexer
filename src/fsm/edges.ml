module type EDGE_TYPE = 
sig
  include X.X_TYPE
  type node
  type label
  val origin : t -> node
  val terminus : t -> node
  val label : t -> label
  val make : node -> node -> label -> t
  val label_compare : label -> label -> int
  val to_dotstring : t -> string
end;;


module Make 
  (Node: X.X_TYPE)
  (Label : X.X_TYPE) 
  (D: Delim.DELIM_TYPE) =

struct 
  type node = Node.t
  type label = Label.t
      
  let name = "Edge"

  type t = {origin:node;terminus:node;label:label} 

  let compare = compare

  let origin x = x.origin 
  let terminus x = x.terminus 
  let label x = x.label 

  let make n1 n2 l = {origin=n1;terminus=n2;label=l;}


  let pair e1 e2 = make
    (Node.pair (origin e1) (origin e2))
    (Node.pair (terminus e1) (terminus e2))
    (Label.pair (label e1) (label e2))


  let to_string e = 
    D.lb^
      (Node.to_string (origin e))^D.delim^
      (Node.to_string (terminus e))^D.delim^
      (Label.to_string (label e))^D.rb  
      
  
  let of_string string =
    (* s looks like: origin,terminus,label *)
    let s = (fst (Mstring.extractBetweenDelimiters string D.lb D.rb 0)) in
    let sl = Mstring.to_stringlist D.delim s in
    if List.length sl <> 3
    then raise (Failure("Edge.of_string:Too many or two few arguments "))
    else
      let origin = try Node.of_string (List.nth sl 0)
	with _ -> raise (Failure("Edge.of_string:Node Origin"))
      in
      let terminus = try Node.of_string (List.nth sl 1)
	with _ -> raise (Failure("Edge.of_string:Node Terminus"))
      in
      let label = try Label.of_string (List.nth sl 2)
	with | (Failure(x)) -> raise (Failure(x))
	  | _ -> raise (Failure("Edge.of_string:Label"))
      in
      make origin terminus label


  let print ?oc:(oc=stdout) e = 
    output_string oc (to_string e);
    output_string oc "\n";
    flush oc
      
  let print_ ?oc:(oc=stdout) e = 
    output_string oc D.lb;
    Node.print_ ~oc (origin e);
    output_string oc D.delim;
    Node.print_ ~oc (terminus e);
    output_string oc D.delim;
    Label.print_ ~oc (label e);
    output_string oc D.rb

  let to_dotstring e = 
    "\""^(Node.to_string (origin e))^"\""^
      " -> "^
      "\""^(Node.to_string (terminus e))^"\""^
      " [label=\""^(Label.to_string (label e))^"\"]\n"

  let label_compare = Label.compare

end
