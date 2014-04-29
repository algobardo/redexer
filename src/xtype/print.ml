let endline to_string ?oc:(oc=stdout) x = 
  output_string oc (to_string x);
  output_string oc "\n";
  flush oc
    
let plain to_string ?oc:(oc=stdout) x = 
  output_string oc (to_string x);
  flush oc
