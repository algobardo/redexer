let name = "Vocab"
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
