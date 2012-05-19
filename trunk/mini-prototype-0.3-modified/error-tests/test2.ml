let times x y = x * y in
let rec fac n = 
        match n with
          1 => 1
        | _ => times (fac n-1) n
        end
