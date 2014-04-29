module type X_TYPE =
  sig
    type t
    val name: string
    val compare: t -> t -> int
    val pair: t -> t -> t
    val of_string: string -> t
    val to_string: t -> string 
    val print: ?oc:out_channel -> t -> unit
    val print_ : ?oc:out_channel -> t -> unit 
  end


  
module type PAIR_TYPE =
sig
  include X_TYPE
  type first
  type second
  val first : t -> first
  val second : t -> second
  val make : first -> second -> t
end


module Make_Pair
  (D: Delim.DELIM_TYPE) 
  (First: X_TYPE)
  (Second : X_TYPE) =

struct 

  type first = First.t
  type second = Second.t
      
  let name = "Pair:"^First.name^","^Second.name

  type t = {first:first;second:second} 

  let compare = compare

  let first x = x.first
  let second x = x.second

  let make f s = {first=f;second=s}

  let pair x1 x2 = make
    (First.pair (first x1) (first x1))
    (Second.pair (second x2) (second x2))

  let to_string x = 
    D.lb^(First.to_string (first x))^D.delim
    ^(Second.to_string (second x))^D.rb  
      
  let of_string string =
    (* string looks like first:second *)
    let s = (fst (Mstring.extractBetweenDelimiters string D.lb D.rb 0)) in
    let sl = Mstring.to_stringlist D.delim s in
    if List.length sl <> 2
    then raise (Failure("Pair.of_string:Too many or two few arguments "))
    else
      let first = try First.of_string (List.nth sl 0)
      with _ -> raise (Failure("Pair.of_string:First"))
      in
      let second = try Second.of_string (List.nth sl 1)
      with _ -> raise (Failure("Pair.of_string:Second"))
      in
      make first second
	
	  
  let print = Print.endline to_string
      
  let print_ ?oc:(oc=stdout) x = 
    output_string oc D.lb;
    First.print_ ~oc (first x);
    output_string oc D.delim;
    Second.print_ ~oc (second x);
    output_string oc D.rb

end

module type TRIPLE_TYPE = 
sig
  include X_TYPE
  type first
  type second
  type third
  val first : t -> first
  val second : t -> second
  val third : t -> third
  val make : first -> second -> third -> t
end;;


module Make_Triple
  (D: Delim.DELIM_TYPE) 
  (First: X_TYPE)
  (Second : X_TYPE) 
  (Third : X_TYPE) =


struct 
  type first = First.t
  type second = Second.t
  type third = Third.t
      
  let name = "Pair:"^First.name^","^Second.name^","^Third.name

  type t = {first:first;second:second;third:third} 

  let compare = compare

  let first x = x.first
  let second x = x.second
  let third x = x.third

  let make f s t = {first=f;second=s;third=t}

  let pair x1 x2 = make
    (First.pair (first x1) (first x1))
    (Second.pair (second x2) (second x2))
    (Third.pair (third x2) (third x2))

  let to_string x = 
    D.lb^(First.to_string (first x))^D.delim
    ^(Second.to_string (second x))^D.delim
    ^(Third.to_string (third x))^D.rb  
      
      
  let of_string string =
    (* s looks like: symbol:second *)
    let s = (fst (Mstring.extractBetweenDelimiters string D.lb D.rb 0)) in
    let sl = Mstring.to_stringlist D.delim s in
    if List.length sl <> 3
    then raise (Failure("Triple.of_string:Too many or two few arguments "))
    else
      let first = try First.of_string (List.nth sl 0)
      with _ -> raise (Failure("Triple.of_string:First"))
      in
      let second = try Second.of_string (List.nth sl 1)
      with _ -> raise (Failure("Triple.of_string:Second"))
      in
      let third = try Third.of_string (List.nth sl 2)
      with _ -> raise (Failure("Triple.of_string:Third"))
      in
      make first second third
	
	  
  let print = Print.endline to_string
      
  let print_ ?oc:(oc=stdout) x = 
    output_string oc D.lb;
    First.print_ ~oc (first x);
    output_string oc D.delim;
    Second.print_ ~oc (second x);
    output_string oc D.delim;
    Third.print_ ~oc (third x);
    output_string oc D.rb

end

