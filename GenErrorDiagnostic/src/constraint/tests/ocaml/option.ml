let func data = 
  match data with
   | None -> ""
   | Some id -> id + 1 in
func None;
func "abc"
